module Storage.HashFS.ConfigFile
    where

import System.Directory
import System.FilePath
import Tools.Config
import Data.Maybe

data ConfigListen = ConfigListen
    { listenPort    :: Int
    , listenKeyFile :: FilePath
    } deriving (Show,Eq)

data ConfigDb = ConfigDb
    { configDbName        :: String
    , configDbType        :: String
    , configDbPath        :: String
    , configDbDescription :: Maybe String
    , configDbKey         :: Maybe FilePath
    , configPreferredExts :: [String]
    } deriving (Show,Eq)

data ConfigDigest = ConfigDigest
    { digestAlgorithm :: String
    } deriving (Show,Eq)

data ConfigFile = ConfigFile
    { configListen :: Maybe ConfigListen
    , configDigest :: ConfigDigest
    , configDbs    :: [ConfigDb]
    } deriving (Show,Eq)

readAt :: FilePath -> IO (Maybe ConfigFile)
readAt fp = do
    cfg <- readConfigPath fp
    return $ Just $ parseCfg [cfg]
  where
    parseCfg cs =
        let dbs = getAllSections cs "db"
         in ConfigFile
                { configListen = Nothing
                , configDigest = parseDigest cs
                , configDbs    = catMaybes $ map parseDbs dbs
                }
    parseDbs kvs =
        case map (kvsGet kvs) ["type", "path", "name"] of
            [Just ty, Just pa, Just na] ->
                Just $ ConfigDb { configDbName = na
                                , configDbType = ty
                                , configDbPath = pa
                                , configDbKey  = kvsGet kvs "key"
                                , configDbDescription = kvsGet kvs "description"
                                , configPreferredExts = maybe [] words $ kvsGet kvs "preferred-exts"
                                }
            _                  ->
                Nothing
    parseDigest cs =
        let alg = get cs "digest" "algorithm"
         in ConfigDigest
                { digestAlgorithm = maybe "sha256" id alg
                }


readSystem :: IO (Maybe ConfigFile)
readSystem = do
    hd <- getHomeDirectory
    readAt (hd </> ".hashfs" </> "config")
