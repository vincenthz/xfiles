{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Storage.HashFS.Meta
    ( MetaProvider
    , Tag(..)
    , Category(..)
    , metaConnect
    , metaCommit
    , metaDigestGetTags
    , metaDigestRemoveTags
    , metaFindDigestsWhich
    , metaTagGetDigests
    , metaFindDigestsNotTagged
    , metaCreateTag
    , metaFindTag
    , metaRenameTag
    , metaAddData
    , metaUpdateData
    , DataProperty(..)
    -- * tagging
    , metaTag
    , metaUntag
    -- * objects
    , TagQuery(..  )
    , DataInfo(..)
    , DataQuery(..)
    , DateOperator(..)
    , DateField(..)
    , DataNumField(..)
    , NumOperator(..)
    , StrOperator(..)
    , dataSelectorQuery
    -- * Date
    , dateFromElapsed
    , dateToElapsed
    -- * query
    , parseQuery
    ) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Trans
import Crypto.Hash
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Word
import Data.Sql
import Data.Either (partitionEithers)
import Data.List (intercalate)
import System.Directory
import Storage.HashFS.Types
import System.Hourglass
import Data.Hourglass hiding (DateTime)
import Data.Char

data MetaProviderSQL = forall conn . IConnection conn => MetaProviderSQL conn

data MetaProvider =
      MetaProviderBackendSQL MetaProviderSQL

instance Show MetaProvider where
    show (MetaProviderBackendSQL _) = "MetaProviderBackendSQL {..}"
instance Eq MetaProvider where -- not quite valid ..
    (==) (MetaProviderBackendSQL {}) (MetaProviderBackendSQL {}) = True

data Category = Group | Person | Location | Other
    deriving (Show,Eq,Ord)

data Tag = Tag (Maybe Category) String
    deriving (Show,Eq,Ord)

newtype DateTime = DateTime Word64
    deriving (Show,Eq)

data DataProperty =
      Security Int
    | Rating Int
    | DirName String
    | FileName String
    | DigestDate Elapsed
    deriving (Show,Eq)

dateFromElapsed :: Elapsed -> DateTime
dateFromElapsed (Elapsed (Seconds v)) = DateTime $ fromIntegral v

dateToElapsed :: DateTime -> Elapsed
dateToElapsed (DateTime t) = Elapsed (Seconds $ fromIntegral t)

printCategory :: Category -> Char
printCategory cat =
    case cat of
        Group  -> 'g'
        Person -> 'p'
        Location -> 'l'
        Other  -> 'o'

parseCategory :: Char -> Maybe Category
parseCategory 'g' = Just Group
parseCategory 'p' = Just Person
parseCategory 'l' = Just Location
parseCategory 'o' = Just Other
parseCategory _   = Nothing

tagFromString :: String -> Tag
tagFromString s =
    case s of
        c:':':r ->
            case parseCategory c of
                Just cat -> Tag (Just cat) r
                Nothing  -> Tag Nothing s
        _ ->
            Tag Nothing s

tagToString :: Tag -> String
tagToString (Tag (Just cat) s) = printCategory cat : ':' : s
tagToString (Tag Nothing    s) = s

-- Local provider create 3 tables
--
-- data: id, hash, datainfo...
-- tag: id, name,
-- tagmap: data(id) x tag(id)

--data IndexData
data IndexTag

newtype Index idx = Index Integer

--type Tag = String

data DataInfo = DataInfo
    { dataSize     :: Word64 -- in bytes
    , dataDate     :: Maybe Elapsed -- unix seconds
    , dataDirName  :: Maybe String
    , dataFileName :: Maybe String
    }
    deriving (Show,Eq)

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


tagName :: Tag -> String
tagName = tagToString

metaConnect :: String -> String -> IO (Either String MetaProvider)
metaConnect ty path =
    case ty of
        "sqlite3" -> Right <$> localMetaCreate path
        _         -> return $ Left ("invalid meta driver: " ++ show ty)

metaCommit :: MetaProvider -> IO ()
metaCommit (MetaProviderBackendSQL sql) = dbCommit sql

metaDigestGetTags :: HashAlgorithm h => MetaProvider -> Digest h -> IO [Tag]
metaDigestGetTags (MetaProviderBackendSQL sql) = dbDigestGetTags sql

metaDigestRemoveTags :: HashAlgorithm h => MetaProvider -> Digest h -> IO ()
metaDigestRemoveTags (MetaProviderBackendSQL sql) = dbDigestRemoveTags sql

metaTagGetDigests :: HashAlgorithm h => MetaProvider -> Tag -> IO [Digest h]
metaTagGetDigests (MetaProviderBackendSQL sql) = dbTagGetDigests sql

metaFindDigestsNotTagged :: HashAlgorithm h => MetaProvider -> IO [Digest h]
metaFindDigestsNotTagged (MetaProviderBackendSQL sql) = dbFindDigestsNotTagged sql

metaFindDigestsWhich :: HashAlgorithm h => MetaProvider -> Maybe DataQuery -> Maybe TagQuery -> IO [Digest h]
metaFindDigestsWhich (MetaProviderBackendSQL sql) = dbGetDigestsWhich sql

metaCreateTag :: MetaProvider -> Tag -> IO (Index IndexTag)
metaCreateTag (MetaProviderBackendSQL sql) = dbCreateTag sql

metaUntag :: HashAlgorithm h => MetaProvider -> Digest h -> Tag -> IO ()
metaUntag (MetaProviderBackendSQL sql) = dbUntag sql

metaTag :: HashAlgorithm h => MetaProvider -> Digest h -> Tag -> IO ()
metaTag (MetaProviderBackendSQL sql) = dbAddTag sql

metaFindTag :: MetaProvider -> Tag -> IO (Maybe (Index IndexTag))
metaFindTag (MetaProviderBackendSQL sql) = dbFindTag sql

metaRenameTag :: MetaProvider -> Tag -> Tag -> IO ()
metaRenameTag (MetaProviderBackendSQL sql) = dbRenameTag sql

metaAddData :: HashAlgorithm h => MetaProvider -> Digest h -> DataInfo -> IO ()
metaAddData (MetaProviderBackendSQL sql) = dbAddData sql

metaUpdateData :: HashAlgorithm h => MetaProvider -> Digest h -> [DataProperty] -> IO ()
metaUpdateData (MetaProviderBackendSQL sql) = dbUpdateData sql

localMetaCreate :: FilePath -> IO MetaProvider
localMetaCreate metaPath = do
    exists <- doesFileExist metaPath
    conn   <- connectSqlite3 metaPath
    if exists
        then checkDatabase conn
        else initializeDatabase conn
    return $ MetaProviderBackendSQL $ MetaProviderSQL conn

checkDatabase :: IConnection conn => conn -> IO ()
checkDatabase conn = do
    r <- quickQuery conn query []
    case r of
        []         -> error "invalid database format. no version found"
        [[sqlVer]] -> checkUpgrade (fromSql sqlVer)
        _          -> error "expecting version lin"
  where
    query = "SELECT sql_schema FROM version"

    checkUpgrade :: Int -> IO ()
    checkUpgrade ver =
        case ver `compare` latestSupported of
            EQ -> return ()
            LT -> error ("database schema need upgrading")
            GT -> error ("unknown version " ++ show ver ++ " of this table. latest known version: " ++ show latestSupported)

latestSupported :: Int
latestSupported = 1

initializeDatabase :: IConnection conn => conn -> IO ()
initializeDatabase conn = do
    mapM_ (flip (run conn) [])
        [ "CREATE TABLE version (sql_schema INTEGER NOT NULL)"
        , sqlCreate (TableName "data")
                [Field "id" "INTEGER PRIMARY KEY"
                ,Field "hash" "VARCHAR(80) NOT NULL"
                ,Field "size" "WORD64" -- size in bytes
                ,Field "itime" "WORD64" -- insertion in the database
                ,Field "date" "WORD64"  -- some date (could be mtime, or some data related time: when it has been made)
                ,Field "dirname" "VARCHAR(1024)"
                ,Field "filename" "VARCHAR(1024)"
                ,Field "security" "INT"
                ,Field "rating" "INT"
                ,Field "parent" "VARCHAR(80)"
                ]
        , "CREATE TABLE tag (id INTEGER PRIMARY KEY, name VARCHAR(128), date WORD64)"
        , "CREATE TABLE tagmap (data_id INTEGER NOT NULL, tag_id INTEGER NOT NULL, UNIQUE(data_id, tag_id) ON CONFLICT REPLACE)"
        , "INSERT INTO version (sql_schema) VALUES (" ++ show latestSupported ++ ")"
        ]
    commit conn

dbCommit :: MetaProviderSQL -> IO ()
dbCommit (MetaProviderSQL conn) = commit conn

dbAddTag :: MetaProviderSQL -> Digest h -> Tag -> IO ()
dbAddTag (MetaProviderSQL conn) digest tag = do
    stmt <- prepare conn query
    void $ execute stmt []
  where query = "INSERT INTO tagmap (data_id, tag_id) SELECT data.id, tag.id FROM data, tag WHERE data.hash = "
            ++ sqlShowString (digestToDb digest) ++ " AND tag.name = " ++ sqlShowString (tagName tag)

dbAddData :: MetaProviderSQL -> Digest h -> DataInfo -> IO ()
dbAddData (MetaProviderSQL conn) digest dataInfo = do
    Elapsed (Seconds c) <- timeCurrent
    stmt <- prepare conn query
    void $ execute stmt [ toSql (digestToDb digest)
                        , toSql (dataSize dataInfo)
                        , toSql c
                        , maybe SqlNull sqlElapsed $ dataDate dataInfo
                        , toSql (maybe "" id $ dataDirName dataInfo)
                        , toSql (maybe "" id $ dataFileName dataInfo)
                        , toSql (0 :: Int)
                        , toSql (0 :: Int)
                        ]
  where
    query = "INSERT INTO data (hash, size, itime, date, dirname, filename, security, rating) VALUES (?,?,?,?,?,?,?,?)" -- hash, size, itime, date, dirname, filename, security, rating

dbUpdateData :: MetaProviderSQL -> Digest h -> [DataProperty] -> IO ()
dbUpdateData (MetaProviderSQL _)    _      []    = return ()
dbUpdateData (MetaProviderSQL conn) digest props = do
    stmt <- prepare conn query
    _    <- execute stmt vs
    return () -- return if the data has been updated or not.
  where
    query = "UPDATE data SET " ++ intercalate ", " ks ++ " WHERE hash = " ++ sqlShowString (digestToDb digest)
    (ks,vs) = (map (eqVal . fst) kvs, map snd kvs)
    kvs     = map toKV props

    eqVal k = k ++ " = ? "

    toKV (Security s)   = ("security", toSql s)
    toKV (Rating i)     = ("rating", toSql i)
    toKV (DirName s)    = ("dirname", toSql s)
    toKV (FileName s)   = ("filename", toSql s)
    toKV (DigestDate e) = ("date", sqlElapsed e)

sqlElapsed :: Elapsed -> SqlValue
sqlElapsed (Elapsed (Seconds s)) = toSql (fromIntegral s :: Word64)

dbDigestRemoveTags :: HashAlgorithm h => MetaProviderSQL -> Digest h -> IO ()
dbDigestRemoveTags conn digest = run_ conn query []
  where
    query = "DELETE FROM tagmap WHERE data_id IN (SELECT id FROM data WHERE hash = " ++ sqlShowString (digestToDb digest) ++ ")"

dbUntag :: HashAlgorithm h => MetaProviderSQL -> Digest h -> Tag -> IO ()
dbUntag conn digest tag = do
    indexTag <- dbResolveTag conn tag
    case indexTag of
        Nothing   -> return ()
        Just iTag -> run_ conn (query iTag) []
  where
    query iTag = mconcat
        ["DELETE FROM tagmap WHERE "
        , "tag_id = " ++ show (getPrimaryKey iTag)
        , " AND "
        , "data_id IN (SELECT id FROM data WHERE hash = " ++ sqlShowString (digestToDb digest) ++ ")"
        ]

-- | Create a specific tag
--
-- will check if tag already exists
dbCreateTag :: MetaProviderSQL -> Tag -> IO (Index IndexTag)
dbCreateTag sqlConn@(MetaProviderSQL conn) tag = do
    mt <- dbFindTag sqlConn tag
    case mt of
        Nothing -> do
            Elapsed (Seconds currentTime) <- timeCurrent
            stmt <- prepare conn queryInsertTag
            insertAndGetID sqlConn stmt [toSql (tagName tag), toSql currentTime]
        Just t  -> return t
  where queryInsertTag = "INSERT INTO tag (name, date) VALUES (?, ?)"

-- | Try to find the key associated to a Tag
--
-- fix SQL escape
dbFindTag :: MetaProviderSQL -> Tag -> IO (Maybe (Index IndexTag))
dbFindTag (MetaProviderSQL conn) tag = do
    r <- quickQuery conn ("SELECT id FROM tag WHERE name=" ++ sqlShowString (tagName tag)) []
    case r of
        []      -> return Nothing
        [[uid]] -> return $ Just $ Index $ fromSql uid
        _       -> error ("dbFindTag: " ++ show tag ++ " unexpected sql output format " ++ show r)

dbRenameTag :: MetaProviderSQL -> Tag -> Tag -> IO ()
dbRenameTag prov@(MetaProviderSQL conn) oldTag newTag = do
    mti <- dbFindTag prov oldTag
    case mti of
        Nothing -> return ()
        Just ti -> do
            let q = "UPDATE tag SET name=" ++ sqlShowString (tagName newTag) ++ " WHERE id = " ++ show (getPrimaryKey ti)
            stmt <- prepare conn q
            _    <- execute stmt []
            return ()

dbDigestGetTags :: MetaProviderSQL -> Digest h -> IO [Tag]
dbDigestGetTags (MetaProviderSQL conn) digest = do
    let query = mconcat
            [ "SELECT tag.name FROM tag where tag.id IN ("
                , "SELECT tagmap.tag_id FROM tagmap WHERE tagmap.data_id = ("
                    , "SELECT data.id FROM data WHERE hash=" ++ sqlShowString (digestToDb digest)
                , ")"
            , ")"
            ]
    map toRet <$> quickQuery conn query []
  where
    toRet :: [SqlValue] -> Tag
    toRet [x] = tagFromString $ fromSql x
    toRet _   = error "dbDigetGetTags: internal error: query returned invalid number of items"

dbTagGetDigests :: HashAlgorithm h => MetaProviderSQL -> Tag -> IO [Digest h]
dbTagGetDigests (MetaProviderSQL conn) tag = do
    let query = mconcat
            [ "SELECT data.hash FROM data where data.id IN ("
                , "SELECT tagmap.data_id FROM tagmap WHERE tagmap.tag_id = ("
                    , "SELECT tag.id FROM tag WHERE name=" ++ sqlShowString (tagName tag)
                , ")"
            , ")"
            ]
    map toRet <$> quickQuery conn query []
  where
    toRet :: HashAlgorithm h => [SqlValue] -> Digest h
    toRet [x] = digestFromDb $ fromSql x
    toRet _   = error "dbDigetGetTags: internal error: query returned invalid number of items"

dbFindDigestsNotTagged :: HashAlgorithm h => MetaProviderSQL -> IO [Digest h]
dbFindDigestsNotTagged (MetaProviderSQL conn) = do
    {-
    let query = mconcat
            [ "SELECT tagmap.data_id FROM tagmap WHERE tagmap.data_id"
            , " NOT IN "
            , "(SELECT data.id FROM data)"
            ]
            -}
    let query = mconcat
            [ "SELECT hash "
            , "FROM   data "
            , "WHERE  NOT EXISTS ( "
            , "   SELECT 1"
            , "   FROM   tagmap"
            , "   WHERE  data.id = tagmap.data_id"
            , ")"
            ]

    map toRet <$> quickQuery conn query []
  where
    toRet :: HashAlgorithm h => [SqlValue] -> Digest h
    toRet [x] = digestFromDb $ fromSql x
    toRet _   = error "dbDigetGetTags: internal error: query returned invalid number of items"

dbGetDigestsWhich :: HashAlgorithm h => MetaProviderSQL -> Maybe DataQuery -> Maybe TagQuery -> IO [Digest h]
dbGetDigestsWhich (MetaProviderSQL _)    Nothing   Nothing  = error "dbGetDigestsWhich: no query specified"
dbGetDigestsWhich (MetaProviderSQL conn) dataQuery tagQuery = do
    let query = mconcat
            [ "SELECT data.hash FROM data where "
            , maybeAnd (\tq -> "data.id IN (SELECT tagmap.data_id FROM tagmap WHERE tagmap.tag_id = ("
                            ++ tagSelectorQuery tq ++ "))") tagQuery
                       (\dq -> dataSelectorQuery dq) dataQuery
            ]
    map toRet <$> quickQuery conn query []
  where
    maybeAnd :: (x -> String) -> Maybe x
             -> (y -> String) -> Maybe y
             -> String
    maybeAnd _  Nothing  _  Nothing  = error "internal error : no query specified"
    maybeAnd _  Nothing  fy (Just y) = fy y
    maybeAnd fx (Just x) _  Nothing  = fx x
    maybeAnd fx (Just x) fy (Just y) = "(" ++ fx x ++ ") AND (" ++ fy y ++ ")"

    toRet :: HashAlgorithm h => [SqlValue] -> Digest h
    toRet [x] = digestFromDb $ fromSql x
    toRet _   = error "dbDigetGetTags: internal error: query returned invalid number of items"

-- | execute a statement (that should be insert)
-- and return the last inserted rowid (primary key)
insertAndGetID :: MetaProviderSQL -> Statement -> [SqlValue] -> IO (Index a)
insertAndGetID (MetaProviderSQL conn) stmt values = do
    _       <- execute stmt values
    [[uid]] <- quickQuery conn "SELECT last_insert_rowid()" []
    return $ Index $ fromSql uid

-- FIXME probably no need to use the hexadecimal version
digestToDb :: Digest h -> String
digestToDb = show

digestFromDb :: HashAlgorithm h => String -> Digest h
digestFromDb = maybe (error "from db not a valid digest") id . inputDigest OutputHex

-- | run a query
run_ :: (Functor m, MonadIO m) => MetaProviderSQL -> String -> [SqlValue] -> m ()
run_ (MetaProviderSQL conn) query args = void $ liftIO $ run conn query args

dbResolveTag :: MetaProviderSQL -> Tag -> IO (Maybe (Index IndexTag))
dbResolveTag (MetaProviderSQL conn) tag = do
    r <- quickQuery conn ("SELECT id FROM tag WHERE name=" ++ sqlShowString (tagName tag)) []
    case r of
        _:_:_   -> error ("duplicate tag instance in database: " ++ show tag)
        [[uid]] -> return $ Just $ Index $ fromSql uid
        _       -> return Nothing

getPrimaryKey :: Index a -> Integer
getPrimaryKey (Index i) = i

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
            "filesize" -> undefined -- DataSize
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

-- escape a string with potential SQL escape to a safe (?) sql string
{- old API / queries : might need reviving

dbResolveDigest :: MetaProviderSQL -> Digest h -> IO (Maybe (Index IndexData))
dbResolveDigest (MetaProviderSQL conn) digest = do
    r <- quickQuery conn ("SELECT id FROM data WHERE hash='" ++ digestToDb digest ++ "'") []
    case r of
        _:_:_   -> error ("duplicate data instance in database: " ++ show digest)
        [[uid]] -> return $ Just $ Index $ fromSql uid
        _       -> return Nothing

-- | add a Tag on some data
dbAddTagRaw :: MetaProviderSQL -> Index IndexData -> Index IndexTag -> IO ()
dbAddTagRaw (MetaProviderSQL conn) key tag = do
    stmt <- prepare conn query
    void $ execute stmt [toSql (getPrimaryKey key),toSql $ getPrimaryKey tag]
  where query = "INSERT INTO tagmap VALUES (?,?)"
-}
