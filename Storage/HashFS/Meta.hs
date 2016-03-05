{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Storage.HashFS.Meta
    ( MetaProvider
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
    , metaGetTags
    , metaAddData
    , metaUpdateData
    , DataProperty(..)
    , DataInfo(..)
    -- * tagging
    , metaTag
    , metaUntag
    -- * objects
    , dataSelectorQuery
    -- * Date
    , dateFromElapsed
    , dateToElapsed
    -- * query
    , parseQuery
    , tagToString
    , tagFromString
    ) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Trans
import Crypto.Hash
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Word
import Data.Sql hiding (Query)
import Data.Either (partitionEithers)
import Data.List (intercalate)
import System.Directory
import Storage.HashFS.Types
import Storage.HashFS.Query
import System.Hourglass
import Data.Hourglass hiding (DateTime)
import Data.Char

data MetaProviderSQL = forall conn . IConnection conn => MetaProviderSQL conn

data MetaProvider h =
      MetaProviderBackendSQL MetaProviderSQL

instance Show (MetaProvider h) where
    show (MetaProviderBackendSQL _) = "MetaProviderBackendSQL {..}"
instance Eq (MetaProvider h) where -- not quite valid ..
    (==) (MetaProviderBackendSQL {}) (MetaProviderBackendSQL {}) = True

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

data RenameStatus = RenameSourceNotExist | RenameDone | RenameMerged
    deriving (Show,Eq)

tagName :: Tag -> String
tagName = tagToString

metaConnect :: HashAlgorithm h => String -> String -> IO (Either String (MetaProvider h))
metaConnect ty path =
    case ty of
        "sqlite3" -> Right <$> localMetaCreate path
        _         -> return $ Left ("invalid meta driver: " ++ show ty)

metaCommit :: HashAlgorithm h => MetaProvider h -> IO ()
metaCommit (MetaProviderBackendSQL sql) = dbCommit sql

metaDigestGetTags :: HashAlgorithm h => MetaProvider h -> Digest h -> IO [Tag]
metaDigestGetTags (MetaProviderBackendSQL sql) = dbDigestGetTags sql

metaDigestRemoveTags :: HashAlgorithm h => MetaProvider h -> Digest h -> IO ()
metaDigestRemoveTags (MetaProviderBackendSQL sql) = dbDigestRemoveTags sql

metaTagGetDigests :: HashAlgorithm h => MetaProvider h -> Tag -> IO [Digest h]
metaTagGetDigests (MetaProviderBackendSQL sql) = dbTagGetDigests sql

metaFindDigestsNotTagged :: HashAlgorithm h => MetaProvider h -> IO [Digest h]
metaFindDigestsNotTagged (MetaProviderBackendSQL sql) = dbFindDigestsNotTagged sql

metaFindDigestsWhich :: HashAlgorithm h => MetaProvider h -> QueryStruct DataQuery -> IO [Digest h]
metaFindDigestsWhich (MetaProviderBackendSQL sql) = dbGetDigestsWhich sql

metaCreateTag :: MetaProvider h -> Tag -> IO (Index IndexTag)
metaCreateTag (MetaProviderBackendSQL sql) = dbCreateTag sql

metaUntag :: HashAlgorithm h => MetaProvider h -> Digest h -> Tag -> IO ()
metaUntag (MetaProviderBackendSQL sql) = dbUntag sql

metaTag :: HashAlgorithm h => MetaProvider h -> Digest h -> Tag -> IO ()
metaTag (MetaProviderBackendSQL sql) = dbAddTag sql

metaFindTag :: MetaProvider h -> Tag -> IO (Maybe (Index IndexTag))
metaFindTag (MetaProviderBackendSQL sql) = dbFindTag sql

metaRenameTag :: MetaProvider h -> Tag -> Tag -> IO RenameStatus
metaRenameTag (MetaProviderBackendSQL sql) = dbRenameTag sql

metaGetTags :: MetaProvider h -> IO [Tag]
metaGetTags (MetaProviderBackendSQL sql) = dbGetTags sql

metaAddData :: HashAlgorithm h => MetaProvider h -> Digest h -> DataInfo -> IO ()
metaAddData (MetaProviderBackendSQL sql) = dbAddData sql

metaUpdateData :: HashAlgorithm h => MetaProvider h -> Digest h -> [DataProperty] -> IO ()
metaUpdateData (MetaProviderBackendSQL sql) = dbUpdateData sql

localMetaCreate :: FilePath -> IO (MetaProvider h)
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

dbGetTags :: MetaProviderSQL -> IO [Tag]
dbGetTags (MetaProviderSQL conn) =
    map toTagName <$> quickQuery conn ("SELECT name FROM tag") []
  where
    toTagName [name] = tagFromString $ fromSql name
    toTagName _      = error ("dbGetTags: unexpected sql output format")

dbRenameTag :: MetaProviderSQL -> Tag -> Tag -> IO RenameStatus
dbRenameTag prov@(MetaProviderSQL conn) oldTag newTag = do
    mti <- dbFindTag prov oldTag
    case mti of
        Nothing -> return RenameSourceNotExist
        Just ti -> do
            mexists <- dbFindTag prov newTag
            -- if already exist then we want to update all the existing tagmap pointing to this tag to the new tag
            case mexists of
                Just exist -> do
                    let q1 = "UPDATE tagmap SET tag_id=" ++ show (getPrimaryKey exist) ++ " WHERE id = " ++ show (getPrimaryKey ti)
                        q2 = "DELETE tag WHERE id=" ++ show (getPrimaryKey ti)
                    -- do something atomic..
                    stmt  <- prepare conn q1
                    _     <- execute stmt []
                    stmt2 <- prepare conn q2
                    _     <- execute stmt2 []
                    return RenameMerged
                Nothing -> do
                    let q = "UPDATE tag SET name=" ++ sqlShowString (tagName newTag) ++ " WHERE id = " ++ show (getPrimaryKey ti)
                    stmt <- prepare conn q
                    _    <- execute stmt []
                    return RenameDone

-- Return all the tags, tagging a specific digest
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

-- Return the digests that are tagged with a specific tag
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

dbGetDigestsWhich :: HashAlgorithm h => MetaProviderSQL -> Query -> IO [Digest h]
dbGetDigestsWhich (MetaProviderSQL conn) dataQuery = do
    let query = mconcat
            [ "SELECT data.hash FROM data where "
            , sqlQuery $ structDataToString dataQuery
            ]
    --putStrLn $ show query
    map toRet <$> quickQuery conn query []
  where
    toRet :: HashAlgorithm h => [SqlValue] -> Digest h
    toRet [x] = digestFromDb $ fromSql x
    toRet _   = error "dbDigetGetTags: internal error: query returned invalid number of items"

    structDataToString x =
        case x of
            StructAnd e1 e2 ->
                structDataToString e1 :&&: structDataToString e2
            StructOr e1 e2  ->
                structDataToString e1 :||: structDataToString e2
            StructExpr e    ->
                case dataSelectorQuery e of
                    Left tq    -> toNested tq
                    Right sqlQ -> sqlQ
    toNested x =
        case x of
            StructAnd e1 e2 -> toNested e1 :&&: toNested e2
            StructOr e1 e2  -> toNested e1 :||: toNested e2
            StructExpr _    ->
                let w = tmTagId `EqQuery` (Select [tId] tTag $ tagSelectorQuery x)
                 in dId `InQuery` Select [tmDataId] tTagmap w

    tData   = TableName "data"
    tTag    = TableName "tag"
    tTagmap = TableName "tagmap"
    dId     = sqlFQFN tData "id"
    tId     = sqlFQFN tTag  "id"
    tmDataId = sqlFQFN tTagmap "data_id"
    tmTagId  = sqlFQFN tTagmap "tag_id"

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
