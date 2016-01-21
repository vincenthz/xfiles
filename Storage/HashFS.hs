{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Rank2Types #-}
module Storage.HashFS
    (
      HashFSConf(..)
    , makeConf
    , makeConfSHA256
    , makeConfSHA512
    , OutputDesc(..)
    , outputDigest
    , inputDigest
    , hashFile
    , hashFileContext
    , withConfig
    {-
    , initializeLocally
    , onDigestFile
    -- * Local
    , deleteFile
    , readFile
    , readInfo
    , verify
    , exists
    , iterateFiles
    , getPath
    -}
    , Local.ImportType(..)
    , importInto
    , importIntoAt
    -- * Generic operations
    , findDigest
    , findAllDigest
    , existsDigest
    , deleteFrom
    , pushFromTo
    -- * Providers
    , Providers
    , Provider(..)
    , ProviderPerm(..)
    , ProviderBackend(..)
    ) where

import           System.Directory
import           Control.Monad
import           Storage.HashFS.Types
import           Storage.HashFS.Path
import           Storage.HashFS.Utils
import           Storage.HashFS.Hasher
import           Storage.HashFS.IO
import           Storage.HashFS.ConfigFile
import           Storage.HashFS.Local (makeConf, makeConfSHA512, makeConfSHA256)
import qualified Storage.HashFS.Local as Local

-- | List of Providers
type Providers h = [Provider h]

{-
data Trunk h = TrunkGroup
    { trunkGroupProviders    :: [Provider h]
    , trunkGroupRouting      :: Int
    }
-}

-- | Generic Provider
data Provider h = Provider
    { providerName        :: String
    , providerDescription :: Maybe String
    , providerPerm        :: ProviderPerm
    , providerBackend     :: ProviderBackend h
    } deriving (Show,Eq)

-- | Provider permission ability: ReadOnly or ReadWrite
data ProviderPerm =
      ReadOnly
    | ReadWrite
    deriving (Show,Eq)

-- | Provider backends : local, or remote
data ProviderBackend h =
      ProviderLocal (HashFSConf h)
    | ProviderRemote Remote
    deriving (Show,Eq)

data Remote = RemoteNative
    deriving (Show,Eq)

hashFileContext :: HashAlgorithm h => FilePath -> IO (Digest h)
hashFileContext fp = hashFile hasherInitContext fp

withConfig :: (forall h . (Show h, HashAlgorithm h) => [Provider h] -> IO ()) -> IO ()
withConfig f = do
    mcfg <- readSystem
    case mcfg of
        Nothing  -> error "no config file. you need to create a \".hashfs/config\" in your home directory"
        Just cfg -> do
            let dbs = configDbs cfg
            case digestAlgorithm $ configDigest cfg of
                "sha224"     -> f =<< mapM (toProvider SHA224) dbs
                "sha256"     -> f =<< mapM (toProvider SHA256) dbs
                "blake2-224" -> f =<< mapM (toProvider Blake2s_224) dbs
                unknown      -> error $ "unknown hash: " ++ unknown
  where
    toProvider :: (Show h, HashAlgorithm h) => h -> ConfigDb -> IO (Provider h)
    toProvider hashAlg cdb = do
        case configDbType cdb of
            "local" -> do
                let confPath = configDbPath cdb
                path <- normalizeLocalPath confPath
                -- check if the directory exists, if not create it
                exist <- doesDirectoryExist path
                unless exist $ createDirectoryIfMissing True path
                -- check if the directory is empty, if not initialize a local database
                isEmpty <- isEmptyDirectory path

                providerLocalConf <- if isEmpty
                        then do
                            let conf = Local.makeConf [2,1] (hasherInit hashAlg) OutputBase32 path
                            Local.initialize conf
                            return conf
                        else
                            Local.start hashAlg path

                return $ Provider (configDbName cdb)
                                  (configDbDescription cdb)
                                  ReadWrite
                                  (ProviderLocal providerLocalConf)
            unknown -> error ("unknown database type: " ++ unknown)

{-
data Routing = Routing
    { filepathPrefixes :: FilePath
    , filepathExtensions :: [String]
    }

sortBy longest
-}

{-
initializeLocally :: (Show h, HashAlgorithm h) => String -> FilePath -> IO (Provider h)
initializeLocally n fp = do
    let conf = Local.makeConfContext [2,1] OutputBase32 fp
    Local.initialize conf
    return $ Provider n Nothing ReadWrite (ProviderLocal conf)
    -}

existsIn :: HashAlgorithm h => ProviderBackend h -> Digest h -> IO Bool
existsIn (ProviderLocal conf) digest = Local.exists conf digest
existsIn (ProviderRemote _)   _      = return False

-- | Find if a provider provides the data related to this digest
findDigest :: HashAlgorithm h => Providers h -> Digest h -> IO (Maybe (Provider h))
findDigest providers digest = loop providers
  where
    loop []     = return Nothing
    loop (x:xs) = do
        r <- existsIn (providerBackend x) digest
        if r then return (Just x) else loop xs

-- | Find all the providers that provides the data related to this digest
findAllDigest :: HashAlgorithm h => Providers h -> Digest h -> IO (Providers h)
findAllDigest providers digest = loop [] providers
  where
    loop acc []     = return $ reverse acc
    loop acc (x:xs) = do
        r <- existsIn (providerBackend x) digest
        loop (if r then x : acc else acc) xs

-- | Check if a data related to a digest exists in one of the providers
existsDigest :: HashAlgorithm h => Providers h -> Digest h -> IO Bool
existsDigest providers digest = maybe False (const True) <$> findDigest providers digest

-- | Import a file into a provider and return the digest
importInto :: HashAlgorithm h => Provider h -> Local.ImportType -> FilePath -> IO (Digest h)
importInto provider importType filePath =
    case providerBackend provider of
        ProviderLocal local -> Local.importFile local importType filePath
        ProviderRemote _    -> error "remote import not implemented"

-- | Import a file into a provider with a specific digest value
importIntoAt :: HashAlgorithm h => Provider h -> Local.ImportType -> Digest h -> FilePath -> IO ()
importIntoAt = error "importIntoAt not implemented"

deleteFrom :: HashAlgorithm h
           => Provider h
           -> Digest h
           -> IO ()
deleteFrom provider digest = do
    case providerBackend provider of
        ProviderLocal dLocal   -> localDelete dLocal
        ProviderRemote dRemote -> remoteDelete dRemote
  where
    localDelete conf =
        Local.deleteFile conf digest
    remoteDelete _ =
        error "remote delete not implemented"

-- | Push from @sourceProvider@ to @destProvider, the data associated with @digest@
pushFromTo :: (Show h, HashAlgorithm h)
           => Provider h -- ^ destination provider
           -> Provider h -- ^ source provider
           -> Digest h   -- ^ digest to push to destination
           -> IO ()
pushFromTo destProvider sourceProvider digest =
    case (providerBackend destProvider, providerBackend sourceProvider) of
        (ProviderLocal dLocal, ProviderLocal sLocal)     -> localToLocal dLocal sLocal
        (ProviderLocal dLocal, ProviderRemote sRemote)   -> pullFromRemote dLocal sRemote
        (ProviderRemote dRemote, ProviderLocal sLocal)   -> pushToRemote dRemote sLocal
        (ProviderRemote dRemote, ProviderRemote sRemote) -> remoteToRemote dRemote sRemote
  where
    -- local to local is a simple file copy
    localToLocal d s = do
        dr       <- Local.hashFileDataReader s digest
        dw       <- Local.hashFileDataWriter d
        computed <- pipeDataDigest dr dw
        when (computed /= digest) $ error ("source provider " ++ show sourceProvider ++ " inconsistent digest: " ++ show digest ++ " got: " ++ show computed)
    -- retrieve a remote file on the local copy
    pullFromRemote _ _ =
        error "pullFromRemote is not implemented yet"
    -- push a remote file from local
    pushToRemote _ _ =
        error "pushToRemote is not implemented yet"
    -- if source and destination can talk to each other, then smart protocol could
    -- make the data be replicated, otherwise this will default to
    -- local retrieval, plus remote sending.
    remoteToRemote _ _ =
        error "remoteToRemote is not implemented yet"
