{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Storage.HashFS.Meta
    ( MetaProvider
    , metaConnect
    , metaCommit
    , metaDigestGetTags
    , metaDigestRemoveTags
    , metaTagGetDigests
    , metaFindDigestsNotTagged
    , metaCreateTag
    , metaFindTag
    , metaAddData
    -- * tagging
    , metaTag
    , metaUntag
    -- * objects
    , DataInfo(..)
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Crypto.Hash
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Word
import System.Directory
import Storage.HashFS.Types
import System.Hourglass
import Data.Hourglass

data MetaProviderSQL = forall conn . IConnection conn => MetaProviderSQL conn

data MetaProvider =
      MetaProviderBackendSQL MetaProviderSQL

instance Show MetaProvider where
    show (MetaProviderBackendSQL _) = "MetaProviderBackendSQL {..}"
instance Eq MetaProvider where -- not quite valid ..
    (==) (MetaProviderBackendSQL {}) (MetaProviderBackendSQL {}) = True

-- Local provider create 3 tables
--
-- data: id, hash, datainfo...
-- tag: id, name,
-- tagmap: data(id) x tag(id)

--data IndexData
data IndexTag

newtype Index idx = Index Integer

type Tag = String

data DataInfo = DataInfo
    { dataSize     :: Word64 -- in bytes
    , dataDate     :: Maybe Word64 -- unix seconds
    , dataDirName  :: Maybe String
    , dataFileName :: Maybe String
    }
    deriving (Show,Eq)

tagName :: Tag -> String
tagName = id

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

metaCreateTag :: MetaProvider -> Tag -> IO (Index IndexTag)
metaCreateTag (MetaProviderBackendSQL sql) = dbCreateTag sql

metaUntag :: HashAlgorithm h => MetaProvider -> Digest h -> Tag -> IO ()
metaUntag (MetaProviderBackendSQL sql) = dbUntag sql

metaTag :: HashAlgorithm h => MetaProvider -> Digest h -> Tag -> IO ()
metaTag (MetaProviderBackendSQL sql) = dbAddTag sql

metaFindTag :: MetaProvider -> Tag -> IO (Maybe (Index IndexTag))
metaFindTag (MetaProviderBackendSQL sql) = dbFindTag sql

metaAddData :: HashAlgorithm h => MetaProvider -> Digest h -> DataInfo -> IO ()
metaAddData (MetaProviderBackendSQL sql) = dbAddData sql

localMetaCreate :: FilePath -> IO MetaProvider
localMetaCreate metaPath = do
    exists <- doesFileExist metaPath
    conn   <- connectSqlite3 metaPath
    unless exists $ initializeTable conn
    return $ MetaProviderBackendSQL $ MetaProviderSQL conn

initializeTable :: IConnection conn => conn -> IO ()
initializeTable conn = do
    mapM_ (flip (run conn) [])
        [ "CREATE TABLE data (id INTEGER PRIMARY KEY, hash VARCHAR(80) NOT NULL, size WORD64, itime WORD64, date WORD64, dirname VARCHAR(4096), filename VARCHAR(1024))"
        , "CREATE TABLE tag (id INTEGER PRIMARY KEY, name VARCHAR(128))"
        , "CREATE TABLE tagmap (data_id INTEGER NOT NULL, tag_id INTEGER NOT NULL, UNIQUE(data_id, tag_id) ON CONFLICT REPLACE)"
        ]
    commit conn

dbCommit :: MetaProviderSQL -> IO ()
dbCommit (MetaProviderSQL conn) = commit conn

dbAddTag :: MetaProviderSQL -> Digest h -> Tag -> IO ()
dbAddTag (MetaProviderSQL conn) digest tag = do
    stmt <- prepare conn query
    void $ execute stmt []
  where query = "INSERT INTO tagmap (data_id, tag_id) SELECT data.id, tag.id FROM data, tag WHERE data.hash = '"
            ++ digestToDb digest ++ "' AND tag.name = '" ++ tag ++ "'"

dbAddData :: MetaProviderSQL -> Digest h -> DataInfo -> IO ()
dbAddData (MetaProviderSQL conn) digest dataInfo = do
    Elapsed (Seconds c) <- timeCurrent
    stmt <- prepare conn query
    void $ execute stmt [ toSql (digestToDb digest)
                        , toSql (dataSize dataInfo)
                        , toSql c
                        , maybe SqlNull toSql $ dataDate dataInfo
                        , toSql (maybe "" id $ dataDirName dataInfo)
                        , toSql (maybe "" id $ dataFileName dataInfo)
                        ]
  where
    query = "INSERT INTO data (hash, size, itime, date, dirname, filename) VALUES (?,?,?,?,?,?)" -- hash, size, itime, date, dirname, filename

dbDigestRemoveTags :: HashAlgorithm h => MetaProviderSQL -> Digest h -> IO ()
dbDigestRemoveTags conn digest = run_ conn query []
  where
    query = "DELETE FROM tagmap WHERE data_id IN (SELECT id FROM data WHERE hash = '" ++ digestToDb digest ++ "')"

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
        , "data_id IN (SELECT id FROM data WHERE hash = '" ++ digestToDb digest ++ "')"
        ]

-- | Create a specific tag
--
-- will check if tag already exists
dbCreateTag :: MetaProviderSQL -> Tag -> IO (Index IndexTag)
dbCreateTag sqlConn@(MetaProviderSQL conn) tag = do
    mt <- dbFindTag sqlConn tag
    case mt of
        Nothing -> do
            stmt <- prepare conn queryInsertTag
            insertAndGetID sqlConn stmt [toSql (tagName tag)]
        Just t  -> return t
  where queryInsertTag = "INSERT INTO tag (name) VALUES (?)"

-- | Try to find the key associated to a Tag
--
-- fix SQL escape
dbFindTag :: MetaProviderSQL -> Tag -> IO (Maybe (Index IndexTag))
dbFindTag (MetaProviderSQL conn) tag = do
    r <- quickQuery conn ("SELECT id FROM tag WHERE name='" ++ tagName tag ++ "'") []
    case r of
        []      -> return Nothing
        [[uid]] -> return $ Just $ Index $ fromSql uid
        _       -> error ("dbFindTag: " ++ show tag ++ " unexpected sql output format " ++ show r)


dbDigestGetTags :: MetaProviderSQL -> Digest h -> IO [Tag]
dbDigestGetTags (MetaProviderSQL conn) digest = do
    let query = mconcat
            [ "SELECT tag.name FROM tag where tag.id IN ("
                , "SELECT tagmap.tag_id FROM tagmap WHERE tagmap.data_id = ("
                    , "SELECT data.id FROM data WHERE hash='" ++ digestToDb digest ++ "'"
                , ")"
            , ")"
            ]
    map toRet <$> quickQuery conn query []
  where
    toRet :: [SqlValue] -> Tag
    toRet [x] = fromSql x
    toRet _   = error "dbDigetGetTags: internal error: query returned invalid number of items"

dbTagGetDigests :: HashAlgorithm h => MetaProviderSQL -> Tag -> IO [Digest h]
dbTagGetDigests (MetaProviderSQL conn) tag = do
    let query = mconcat
            [ "SELECT data.hash FROM data where data.id IN ("
                , "SELECT tagmap.data_id FROM tagmap WHERE tagmap.tag_id = ("
                    , "SELECT tag.id FROM tag WHERE name='" ++ tag ++ "'"
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
    r <- quickQuery conn ("SELECT id FROM tag WHERE name='" ++ tagName tag ++ "'") []
    case r of
        _:_:_   -> error ("duplicate tag instance in database: " ++ show tag)
        [[uid]] -> return $ Just $ Index $ fromSql uid
        _       -> return Nothing

getPrimaryKey :: Index a -> Integer
getPrimaryKey (Index i) = i

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
