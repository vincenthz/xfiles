{-# LANGUAGE EmptyDataDecls #-}
module Storage.HashFS.Meta
    ( MetaProvider
    , localMetaCreate
    , dbDigestGetTags
    , dbTagGetDigests
    , dbFindDigestsNotTagged
    , dbCommit
    -- * Tag
    , dbCreateTag
    , dbRemoveTag
    , dbFindTag
    -- * objects
    , DataInfo(..)
    , dbAddData
    , dbAddTag
    -- * low level
    , dbResolveDigest
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

data MetaProvider = MetaProvider
    { metaDB :: MetaDB
    }

-- Local provider create 3 tables
--
-- one data:

data IndexData
data IndexTag

newtype Index idx = Index Integer

type Tag = String

type MetaDB = Connection

tagName :: Tag -> String
tagName = id

localMetaCreate :: FilePath -> IO MetaDB
localMetaCreate metaPath = do
    exists <- doesFileExist metaPath
    conn   <- connectSqlite3 metaPath
    unless exists $ initializeTable conn
    return conn

initializeTable conn = do
    mapM_ (flip (run conn) [])
        [ "CREATE TABLE data (id INTEGER PRIMARY KEY, hash VARCHAR(80) NOT NULL, size WORD64, itime WORD64, date WORD64, dirname VARCHAR(4096), filename VARCHAR(1024))"
        , "CREATE TABLE tag (id INTEGER PRIMARY KEY, name VARCHAR(128))"
        , "CREATE TABLE tagmap (data_id INTEGER NOT NULL, tag_id INTEGER NOT NULL, UNIQUE(data_id, tag_id) ON CONFLICT REPLACE)"
        ]
    commit conn

getPrimaryKey :: Index a -> Integer
getPrimaryKey (Index i) = i

dbCommit conn = commit conn

-- | add a Tag on some data
dbAddTagRaw :: Connection -> Index IndexData -> Index IndexTag -> IO ()
dbAddTagRaw conn key tag = do
    stmt <- prepare conn query
    void $ execute stmt [toSql (getPrimaryKey key),toSql $ getPrimaryKey tag]
  where query = "INSERT INTO tagmap VALUES (?,?)"

dbAddTag :: Connection -> Digest h -> Tag -> IO ()
dbAddTag conn hash tag = do
    stmt <- prepare conn query
    void $ execute stmt []
  where query = "INSERT INTO tagmap (data_id, tag_id) SELECT data.id, tag.id FROM data, tag WHERE data.hash = '"
            ++ digestToDb hash ++ "' AND tag.name = '" ++ tag ++ "'"

data DataInfo = DataInfo
    { dataSize :: Word64 -- in bytes
    , dataDate :: Maybe Word64 -- unix seconds
    , dataDirName :: Maybe String
    , dataFileName :: Maybe String
    }

dbAddData :: Connection -> Digest h -> DataInfo -> IO ()
dbAddData conn digest dataInfo = do
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

-- | Remove a Tag from some data
dbRemoveTag :: Connection -> Index IndexData -> Index IndexTag -> IO ()
dbRemoveTag conn key tag = run_ conn query []
  where
    query = "DELETE FROM tagmap WHERE data_id=" ++ show (getPrimaryKey key) ++ " AND tag_id=" ++ show (getPrimaryKey tag)

-- | Create a specific tag
--
-- will check if tag already exists
dbCreateTag :: Connection -> Tag -> IO (Index IndexTag)
dbCreateTag conn tag = do
    mt <- dbFindTag conn tag
    case mt of
        Nothing -> do
            stmt <- prepare conn queryInsertTag
            insertAndGetID conn stmt [toSql (tagName tag)]
        Just t  -> return t
  where queryInsertTag = "INSERT INTO tag (name) VALUES (?)"

-- | Try to find the key associated to a Tag
--
-- fix SQL escape
dbFindTag :: Connection -> Tag -> IO (Maybe (Index IndexTag))
dbFindTag conn tag = do
    r <- quickQuery conn ("SELECT id FROM tag WHERE name='" ++ tagName tag ++ "'") []
    case r of
        []      -> return Nothing
        [[uid]] -> return $ Just $ Index $ fromSql uid
        _       -> error ("dbFindTag: " ++ show tag ++ " unexpected sql output format " ++ show r)

dbResolveDigest :: Connection -> Digest h -> IO (Maybe (Index IndexData))
dbResolveDigest conn digest = do
    r <- quickQuery conn ("SELECT id FROM data WHERE hash='" ++ digestToDb digest ++ "'") []
    case r of
        _:_:_   -> error ("duplicate data instance in database: " ++ show digest)
        [[uid]] -> return $ Just $ Index $ fromSql uid
        _       -> return Nothing


dbDigestGetTags :: Connection -> Digest h -> IO [Tag]
dbDigestGetTags conn digest = do
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

dbTagGetDigests :: HashAlgorithm h => Connection -> Tag -> IO [Digest h]
dbTagGetDigests conn tag = do
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

dbFindDigestsNotTagged :: HashAlgorithm h => Connection -> IO [Digest h]
dbFindDigestsNotTagged conn = do
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

-- | run a query
run_ :: (Functor m, MonadIO m) => Connection -> String -> [SqlValue] -> m ()
run_ conn query args = void $ liftIO $ run conn query args

-- | execute a statement (that should be insert)
-- and return the last inserted rowid (primary key)
insertAndGetID :: IConnection con => con -> Statement -> [SqlValue] -> IO (Index a)
insertAndGetID conn stmt values = do
    _       <- execute stmt values
    [[uid]] <- quickQuery conn "SELECT last_insert_rowid()" []
    return $ Index $ fromSql uid

-- FIXME probably no need to use the hexadecimal version
digestToDb :: Digest h -> String
digestToDb = show

digestFromDb :: HashAlgorithm h => String -> Digest h
digestFromDb = maybe (error "from db not a valid digest") id . inputDigest OutputHex
