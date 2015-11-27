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
    { listenDbType :: String
    , listenDbPath :: String
    , listenDbKey  :: Maybe FilePath
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
        case map (kvsGet kvs) ["type", "path"] of
            [Just ty, Just pa] ->
                Just $ ConfigDb { listenDbType = ty
                                , listenDbPath = pa
                                , listenDbKey  = kvsGet kvs "key"
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
    readAt (hd </> ".hashfs.conf")
