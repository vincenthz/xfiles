module Storage.HashFS.Query
    ( TagQuery(..)
    , DataQuery(..)
    , Query
    , DateOperator(..)
    , DateField(..)
    , DataNumField(..)
    , NumOperator(..)
    , StrOperator(..)
    , QueryStruct(..)
    -- * parsing from string
    , dataSelectorQuery
    , tagSelectorQuery
    , tagSelectorQueryString
    , parseQuery
    ) where

import           Control.Applicative
import           Control.Arrow (first)
import           Storage.HashFS.Types
import           Data.Either (partitionEithers)
import           Data.Char
import           Data.Sql hiding (Query)
import qualified Data.Sql as Sql
import           Text.Read

data QueryStruct expr =
      StructAnd (QueryStruct expr) (QueryStruct expr)
    | StructOr (QueryStruct expr) (QueryStruct expr)
    | StructExpr expr
    deriving (Show,Eq)

-- try to "lift" the Left as far as possible, otherwise apply the a -> b transformation to turn into a Right
queryEitherRight :: (QueryStruct a -> QueryStruct b)
                 -> QueryStruct (Either a b)
                 -> Either (QueryStruct a) (QueryStruct b)
queryEitherRight f (StructAnd e1 e2) =
    case (queryEitherRight f e1, queryEitherRight f e2) of
        (Left x1, Left x2)   -> Left (StructAnd x1 x2)
        (Right x1, Right x2) -> Right (StructAnd x1 x2)
        (Left x1, Right x2)  -> Right (StructAnd (f x1) x2)
        (Right x1, Left x2)  -> Right (StructAnd x1 (f x2))
queryEitherRight f (StructOr e1 e2) =
    case (queryEitherRight f e1, queryEitherRight f e2) of
        (Left x1, Left x2)   -> Left (StructOr x1 x2)
        (Right x1, Right x2) -> Right (StructOr x1 x2)
        (Left x1, Right x2)  -> Right (StructOr (f x1) x2)
        (Right x1, Left x2)  -> Right (StructOr x1 (f x2))
queryEitherRight _ (StructExpr (Left a))  = Left (StructExpr a)
queryEitherRight _ (StructExpr (Right b)) = Right (StructExpr b)

data TagQuery =
      TagEqual    Tag
    | TagNotEqual Tag
    | TagCat      Category
    | TagLike     Category String
    deriving (Show,Eq)

data DataNumField = FieldRating | FieldSecurity
    deriving (Show,Eq)

data NumOperator =
      (:==)
    | (:/=)
    | (:<)
    | (:>)
    | (:<=)
    | (:>=)
    deriving (Show,Eq)

data StrOperator = Contains | StartWith | EndsWith
    deriving (Show,Eq)

data DateOperator =
      Before  Included DateTime
    | After   Included DateTime
    | Between Included DateTime Included DateTime
    deriving (Show,Eq)

data Included = Included | NotIncluded
    deriving (Show,Eq)

--type Date = Word64
data DateField = Itime | Mtime
    deriving (Show,Eq)

data DataQuery =
      DataNum      DataNumField NumOperator Integer
    | DataSize     NumOperator Integer
    | DataFilename StrOperator String
    | DataDate     DateField DateOperator
    | DataTag      (QueryStruct TagQuery)
    deriving (Show,Eq)

type Query = QueryStruct DataQuery

dataSelectorQuery :: DataQuery -> Either (QueryStruct TagQuery) Sql.WhereQuery
dataSelectorQuery = transform
  where
    table    = TableName "data"
    rating   = sqlFQFN table "rating"
    security = sqlFQFN table "security"
    filename = sqlFQFN table "filename"
    mtime    = sqlFQFN table "mtime"
    filesize = sqlFQFN table "size"
    itime    = sqlFQFN table "itime"

    transform (DataTag tq) = Left tq
    transform (DataNum fieldTy numOp v) =
        let field = case fieldTy of
                    FieldRating   -> rating
                    FieldSecurity -> security
         in numTransform field numOp v
    transform (DataSize numOp v) = numTransform filesize numOp v
    transform (DataFilename strOp s) = Right $
        case strOp of
            Contains  -> filename :~~: ("%" ++ s ++ "%")
            StartWith -> filename :~~: (s ++ "%")
            EndsWith  -> filename :~~: ("%" ++ s)
    transform (DataDate dateField dateOp) =
        let f = case dateField of
                    Mtime -> mtime
                    Itime -> itime
         in Right $ case dateOp of
                Before NotIncluded d     -> f :<: elapsedToInt d
                Before Included d        -> f :<=: elapsedToInt d
                After NotIncluded d      -> f :>: elapsedToInt d
                After Included d         -> f :>=: elapsedToInt d
                Between ic1 d1 ic2 d2    ->
                         if ic1 == Included then (f :>=: elapsedToInt d1) else (f :>: elapsedToInt d1)
                    :&&: if ic2 == Included then (f :<=: elapsedToInt d2) else (f :<: elapsedToInt d2)

    numTransform field numOp v = Right $
        case numOp of
            (:==) -> field :==: (ValInt v)
            (:/=) -> field :/=: (ValInt v)
            (:<=) -> field :<=: v
            (:>=) -> field :>=: v
            (:<)  -> field :<: v
            (:>)  -> field :>: v

    elapsedToInt :: DateTime -> Integer
    elapsedToInt (DateTime s) = fromIntegral s

tagSelectorQuery :: QueryStruct TagQuery -> Sql.WhereQuery
tagSelectorQuery tq = transformS tq
  where
    n = sqlFN "tag.name"
    transformS (StructAnd q1 q2) = transformS q1 :&&: transformS q2
    transformS (StructOr q1 q2)  = transformS q1 :||: transformS q2
    transformS (StructExpr e)    = transformQ e
    transformQ (TagEqual t)      = n :==: ValString (tagToString t)
    transformQ (TagNotEqual t)   = n :/=: ValString (tagToString t)
    transformQ (TagLike c t)     = n :~~: (printCategory c : ":" ++ t)
    transformQ (TagCat c)        = n :~~: (printCategory c : ":%")

tagSelectorQueryString :: QueryStruct TagQuery -> String
tagSelectorQueryString =
    ("SELECT tag.id FROM tag WHERE " ++) . sqlQuery . tagSelectorQuery

-- | Parse a string representing a query for this meta system
--
-- Example:
-- * tag = abc
-- * tag = abc && person = Alice && person != Bob
-- * group ~= "Holidays*" && (person = Alice || person = Bob) && filesize > 10000 && (security = 2 || rating > 3)
-- * tag = {abc,def,xyz}
-- * person = Alice || (person == Bob && filesize > 10) && group = "no place like home"
--
parseQuery :: String -> Either String Query
parseQuery queryString =
    case runStream parseAtoms $ atomize queryString of
        Right (a, [])         -> do
            r <- parseTagOrData a
            case queryEitherRight (StructExpr . DataTag) r of
                Left x  -> Right $ StructExpr $ DataTag x
                Right y -> Right y
        Right (_, _:_)        -> Left "unparsed content"
        Left err              -> Left ("parse error: " ++ err)
  where
    parseTagOrData :: AtomExpr -> Either String (QueryStruct (Either TagQuery DataQuery))
    parseTagOrData (AtomAnd l)       = do
        r <- mapM parseTagOrData l
        case r of
            []  -> Left "parseTagOrData: empty result in And"
            [x] -> Right x
            _:_ -> Right $ foldl1 StructAnd r

    parseTagOrData (AtomOr l)        = do
        r <- mapM parseTagOrData l
        case r of
            []  -> Left "parseTagOrData: empty result in Or"
            [x] -> Right x
            _:_ -> Right $ foldl1 StructOr r
    parseTagOrData (AtomGroup l)     =
        parseTagOrData l
    parseTagOrData (AtomPred k op v)
        | isTag k   = toTagQuery k op v >>= \tq -> Right $ StructExpr $ Left tq
        | isData k  = toDataQuery k op v >>= \dq -> Right $ StructExpr $ Right dq
        | otherwise = Left "unknown type of expression"

    toDataQuery k op v =
        case k of
            "size"     -> DataSize <$> parseNumOp op <*> parseSize v -- case op of -- DataSize
            "filesize" -> DataSize <$> parseNumOp op <*> parseSize v -- case op of -- DataSize
            "security" -> parseNum FieldSecurity op v
            "rating"   -> parseNum FieldRating op v
            "date"     -> Left ("date query not implemented")
            _          -> Left ("unknown data query: " ++ k)

    parseNum f op v =
        DataNum f <$> parseNumOp op <*> parseInteger v

    parseNumOp op =
        case op of
            "="  -> Right (:==)
            "==" -> Right (:==)
            "/=" -> Right (:/=)
            "!=" -> Right (:/=)
            "<"  -> Right (:<)
            ">"  -> Right (:>)
            "<=" -> Right (:<=)
            ">=" -> Right (:>=)
            _    -> Left ("unknown operator: " ++ show op)

    parseSize v =
        case span isDigit v of
            ("", _)   -> Left ("invalid size: " ++ v)
            (n , "")  -> Right $ read n
            (n, "k")  -> Right $ read n * 1024
            (n, "K")  -> Right $ read n * 1024
            (n, "m")  -> Right $ read n * 1024 * 1024
            (n, "M")  -> Right $ read n * 1024 * 1024
            (n, "g")  -> Right $ read n * 1024 * 1024 * 1024
            (n, "G")  -> Right $ read n * 1024 * 1024 * 1024
            (n, qual) -> Left ("unknown size suffix : " ++ show qual ++ " after number: " ++ show n)
    parseInteger v =
        case readMaybe v of
            Just n  -> Right n
            Nothing -> Left ("invalid number: " ++ show v)

    toTagQuery k op v = do
        cat <- getGroup
        case op of
            "="  -> Right $ TagEqual $ Tag (Just cat) v
            "==" -> Right $ TagEqual $ Tag (Just cat) v
            "!=" -> Right $ TagNotEqual $ Tag (Just cat) v
            "/=" -> Right $ TagNotEqual $ Tag (Just cat) v
            "~=" -> Right $ TagLike cat v
            _    -> Left ("unknown tag operator : " ++ show op ++ " after " ++ show k)
      where
        getGroup =
            case k of
                "person"   -> Right Person
                "location" -> Right Location
                "group"    -> Right Group
                "tag"      -> Right Other
                _          -> Left ("unknown tag category:" ++ k)

    isData = flip elem dataFields
    isTag  = flip elem tagFields
    dataFields = ["filesize", "size", "security", "rating", "date"]
    tagFields = ["person", "location", "group", "tag"]

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
      AtomAnd   [AtomExpr]
    | AtomOr    [AtomExpr]
    | AtomGroup AtomExpr
    | AtomPred  String String String
    deriving (Show,Eq)

data Atom = AtomInt        String
          | AtomOperator   String
          | AtomSymbol     String
          | AtomString     String
          | AtomError      Char
          | AtomParseError String
        deriving (Show,Eq)

eatRet :: Show elem => (elem -> Maybe a) -> Stream elem a
eatRet predicate = Stream $ \el ->
    case el of
        []   -> Left ("empty stream: eating")
        x:xs ->
            case predicate x of
                Just a  -> Right (a, xs)
                Nothing -> Left ("unexpected atom got: " ++ show x)

eat :: Show elem => (elem -> Bool) -> Stream elem ()
eat predicate = Stream $ \el ->
    case el of
        [] -> Left ("empty stream: eating")
        x:xs
            | predicate x -> Right ((), xs)
            | otherwise   -> Left ("unexpected atom got: " ++ show x)

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
