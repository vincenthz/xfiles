module Storage.HashFS.Query
    ( TagQuery(..)
    , DataQuery(..)
    , DateOperator(..)
    , DateField(..)
    , DataNumField(..)
    , NumOperator(..)
    , StrOperator(..)
    -- * parsing from string
    , dataSelectorQuery
    , tagSelectorQuery
    , parseQuery
    ) where

import           Control.Applicative
import           Control.Arrow (first)
import           Storage.HashFS.Types
import           Data.Either (partitionEithers)
import           Data.Char
import           Data.Sql

data TagQuery =
      TagEqual Tag
    | TagNotEqual Tag
    | TagCat Category
    | TagLike Category String
    | Or TagQuery TagQuery
    | And TagQuery TagQuery

data DataNumField = FieldRating | FieldSecurity
    deriving (Show,Eq)

data NumOperator =
      (:==) DataNumField
    | (:/=) DataNumField
    | (:<) DataNumField
    | (:>) DataNumField
    | (:<=) DataNumField
    | (:>=) DataNumField
    deriving (Show,Eq)

data StrOperator = Contains | StartWith | EndsWith
    deriving (Show,Eq)

data DateOperator =
      Before Included DateTime
    | After Included DateTime
    | Between Included DateTime Included DateTime
    deriving (Show,Eq)

data Included = Included | NotIncluded
    deriving (Show,Eq)

--type Date = Word64
data DateField = Itime | Mtime
    deriving (Show,Eq)

data DataQuery =
      DataNum NumOperator Int
    | DataFilename StrOperator String
    | DataDate DateField DateOperator
    | DataAnd DataQuery DataQuery
    | DataOr DataQuery DataQuery

dataSelectorQuery :: DataQuery -> String
dataSelectorQuery = sqlQuery . transform
  where
    table    = TableName "data"
    rating   = sqlFQFN table "rating"
    security = sqlFQFN table "security"
    filename = sqlFQFN table "filename"
    mtime    = sqlFQFN table "mtime"
    itime    = sqlFQFN table "itime"

    transform (DataOr d1 d2)          = transform d1 :||: transform d2
    transform (DataAnd d1 d2)         = transform d1 :&&: transform d2
    transform (DataNum numOp v) =
        let f field = case field of
                    FieldRating   -> rating
                    FieldSecurity -> security
         in case numOp of
                (:==) field -> f field :==: (ValInt v)
                (:/=) field -> f field :/=: (ValInt v)
                (:<=) field -> f field :<=: v
                (:>=) field -> f field :>=: v
                (:<)  field -> f field :<: v
                (:>)  field -> f field :>: v
    transform (DataFilename strOp s) =
        case strOp of
            Contains  -> filename :~~: ("%" ++ s ++ "%")
            StartWith -> filename :~~: (s ++ "%")
            EndsWith  -> filename :~~: ("%" ++ s)
    transform (DataDate dateField dateOp) =
        let f = case dateField of
                    Mtime -> mtime
                    Itime -> itime
         in case dateOp of
                Before NotIncluded d     -> f :<: elapsedToInt d
                Before Included d        -> f :<=: elapsedToInt d
                After NotIncluded d      -> f :>: elapsedToInt d
                After Included d         -> f :>=: elapsedToInt d
                Between ic1 d1 ic2 d2    ->
                         if ic1 == Included then (f :>=: elapsedToInt d1) else (f :>: elapsedToInt d1)
                    :&&: if ic2 == Included then (f :<=: elapsedToInt d2) else (f :<: elapsedToInt d2)

    elapsedToInt :: DateTime -> Int
    elapsedToInt (DateTime s) = fromIntegral s

tagSelectorQuery :: TagQuery -> String
tagSelectorQuery query =
    "SELECT tag.id FROM tag WHERE " ++ sqlQuery (transform query)
  where
    n = sqlFN "name"

    transform (And q1 q2)     = transform q1 :&&: transform q2
    transform (Or q1 q2)      = transform q1 :||: transform q2
    transform (TagEqual t)    = n :==: ValString (tagToString t)
    transform (TagNotEqual t) = n :/=: ValString (tagToString t)
    transform (TagLike c t)   = n :~~: (printCategory c : ":" ++ t)
    transform (TagCat c)      = n :~~: (printCategory c : ":%")

-- | Parse a string representing a query for this meta
--
-- Example:
-- * tag = abc
-- * tag = abc && person = Alice && person != Bob
-- * group ~= "Holidays*" && (person = Alice || person = Bob) && filesize > 10000 && (security = 2 || rating > 3)
-- * tag = {abc,def,xyz}
--
parseQuery :: String -> Either String (Maybe DataQuery, Maybe TagQuery)
parseQuery queryString =
    case runStream parseAtoms $ atomize queryString of
        Right (AtomAnd l, []) -> parseQueryAnd l
        Right (a, [])         -> parseQueryAnd [a]
        Right (_, _:_)        -> Left "unparsed content"
        Left err              -> Left ("parse error: " ++ err)
  where
    parseQueryAnd :: [AtomExpr] -> Either String (Maybe DataQuery, Maybe TagQuery)
    parseQueryAnd es =
        let (errs, qs)   = partitionEithers $ map parseQueryInner es
            (dats, tags) = partitionEithers qs
         in case errs of
             [] -> Right
                ( foldl (\acc q -> maybe (Just q) (undefined) acc) Nothing dats
                , foldl (\acc q -> maybe (Just q) (undefined) acc) Nothing tags)
             e:_  -> Left e

    parseQueryInner :: AtomExpr
                    -> Either String (Either DataQuery TagQuery)
    parseQueryInner e
        | isData e  = either Left (Right . Left) $ toDataQuery e
        | isTag e   = either Left (Right . Right) $ toTagQuery e
        | otherwise = Left ("atom not a valid data or tag query: " ++ show e)
        {-
        data DataQuery =
              DataNum NumOperator Int
            | DataFilename StrOperator String
            | DataDate DateField DateOperator
            | DataAnd DataQuery DataQuery
            | DataOr DataQuery DataQuery
        -}
    toDataQuery :: AtomExpr -> Either String DataQuery
    toDataQuery (AtomAnd l)       =
        case mapM toDataQuery l of
            Left err     -> Left err
            Right []     -> Left "toDataQuery: and: empty"
            Right (x:xs) -> Right $ foldl DataAnd x xs
    toDataQuery (AtomOr l)        =
        case mapM toDataQuery l of
            Left err     -> Left err
            Right []     -> Left "toDataQuery: or: empty"
            Right (x:xs) -> Right $ foldl DataOr x xs
    toDataQuery (AtomGroup l)     = toDataQuery l
    toDataQuery (AtomPred k op v) =
        case k of
            "filesize" -> undefined -- case op of -- DataSize
            "security" -> undefined
            "rating"   -> undefined
            "date"     -> undefined
            _          -> Left ("unknown data atom: " ++ k)

    toTagQuery :: AtomExpr -> Either String TagQuery
    toTagQuery (AtomAnd l)       = undefined
    toTagQuery (AtomOr l)        = undefined
    toTagQuery (AtomGroup l)     = undefined
    toTagQuery (AtomPred k op v) = undefined

    isData = atomRec $ flip elem dataFields
    isTag  = atomRec $ flip elem tagFields
    dataFields = ["filesize", "security", "rating", "date"]
    tagFields = ["person", "location", "group"]
    atomRec f (AtomAnd l)      = all (atomRec f) l
    atomRec f (AtomOr l)       = all (atomRec f) l
    atomRec f (AtomGroup g)    = atomRec f g
    atomRec f (AtomPred k _ _) = f k

    parseAtoms = parseAnd
    parseAnd = do
        e1 <- parseOr
        es <- many (eat (== AtomOperator "&&") >> parseOr)
        if null es
            then return e1
            else return $ AtomAnd (e1 : es)
    parseOr = do
        e1 <- parseExpr
        es <- many (eat (== AtomOperator "||") >> parseExpr)
        if null es
            then return e1
            else return $ AtomOr (e1 : es)
    parseExpr = do
            AtomGroup <$> (eat (== AtomOperator "(") *> parseAnd <* eat (== AtomOperator ")"))
        <|> parseKv
    parseKv = do
        k <- eatRet $ \e -> case e of
                        AtomSymbol s -> Just s
                        AtomString s -> Just s
                        _            -> Nothing
        op <- eatRet $ \e -> case e of
                AtomOperator op -> Just op
                _               -> Nothing
        v <- eatRet $ \e -> case e of
                        AtomSymbol s -> Just s
                        AtomString s -> Just s
                        AtomInt i    -> Just i
                        _            -> Nothing
        return $ AtomPred k op v
{-
    and_expr := or_expr [&& or_expr ...]
    or_expr  := expr [|| expr ...]
    expr     := ( and_expr )
             |  ty operator value
    ty       = Symbol | String
    value    = String | Symbol | Int | List
    operator = ~= | = | != | /= | > | >= | < | <=
-}

    atomize []         = []
    atomize l@(x:xs)
        | isDigit x    = eatConstruct l AtomInt isDigit
        | isSpace x    = atomize xs
        | isOperator x = eatConstruct l AtomOperator isOperator
        | x == '"'     = eatString [] xs
        | isAlpha x    = eatConstruct l AtomSymbol isAlphaNum
        | otherwise    = AtomError x : atomize xs

    isOperator = flip elem "=!/&|(){}<>~"

    eatConstruct l constr f =
        let (xs1, xs2) = break (not . f) l
         in constr xs1 : atomize xs2
    eatString acc []             = AtomParseError ("unterminated string: " ++ show ('"' : reverse acc)) : []
    eatString acc ('"':xs)       = AtomString (reverse acc) : atomize xs
    eatString acc ('\\':'"':xs)  = eatString ('"' : acc) xs
    eatString acc ('\\':'\\':xs) = eatString ('\\': acc) xs
    eatString acc ('\\':xs)      = eatString ('\\': acc) xs
    eatString acc (x:xs)         = eatString (x : acc) xs

data AtomExpr =
      AtomAnd [AtomExpr]
    | AtomOr  [AtomExpr]
    | AtomGroup AtomExpr
    | AtomPred String String String
    deriving (Show,Eq)

data Atom = AtomInt String
          | AtomOperator String
          | AtomSymbol String
          | AtomString String
          | AtomError Char
          | AtomParseError String
        deriving (Show,Eq)

eatRet :: Show elem => (elem -> Maybe a) -> Stream elem a
eatRet predicate = Stream $ \el ->
    case el of
        []           -> Left ("empty stream: eating")
        x:xs ->
            case predicate x of
                Just a  -> Right (a, xs)
                Nothing -> Left ("unexpected atom got: " ++ show x)

eat :: Show elem => (elem -> Bool) -> Stream elem ()
eat predicate = Stream $ \el ->
    case el of
        []           -> Left ("empty stream: eating")
        x:xs
            | predicate x    -> Right ((), xs)
            | otherwise -> Left ("unexpected atom got: " ++ show x)

newtype Stream elem a = Stream { runStream :: [elem] -> Either String (a, [elem]) }
instance Functor (Stream elem) where
    fmap f s = Stream $ \e1 -> case runStream s e1 of
        Left err     -> Left err
        Right (a,e2) -> Right (f a, e2)
instance Applicative (Stream elem) where
    pure  = return
    fab <*> fa = Stream $ \e1 -> case runStream fab e1 of
        Left err      -> Left err
        Right (f, e2) -> either Left (Right . first f) $ runStream fa e2
instance Alternative (Stream elem) where
    empty     = Stream $ \_  -> Left "empty"
    f1 <|> f2 = Stream $ \e1 -> either (\_ -> runStream f2 e1) Right $ runStream f1 e1
instance Monad (Stream elem) where
    return a  = Stream $ \e1 -> Right (a, e1)
    ma >>= mb = Stream $ \e1 -> either Left (\(a, e2) -> runStream (mb a) e2) $ runStream ma e1
