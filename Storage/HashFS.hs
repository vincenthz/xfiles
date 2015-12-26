{-# LANGUAGE ViewPatterns #-}
module Storage.HashFS
    (
      HashFSConf(..)
    , makeConf
    , makeConfContext
    , makeConfSHA256
    , makeConfSHA512
    , OutputDesc(..)
    , outputDigest
    , inputDigest
    , hashFile
    , hashFileContext
    , initializeLocally
    {-
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
    , find
    , findAll
    , exists
    , pushFromTo
    -- * Providers
    , Providers
    , Provider(..)
    , ProviderPerm(..)
    , ProviderBackend(..)
    ) where

import           Control.Monad
import           Storage.HashFS.Types
import           Storage.HashFS.Path
import           Storage.HashFS.Utils
import           Storage.HashFS.Hasher
import           Storage.HashFS.IO
import           Storage.HashFS.Local (makeConf, makeConfContext, makeConfSHA512, makeConfSHA256)
import qualified Storage.HashFS.Local as Local

-- | List of Providers
type Providers h = [Provider h]

-- | Generic Provider
data Provider h = Provider
    { providerName    :: String
    , providerPerm    :: ProviderPerm
    , providerBackend :: ProviderBackend h
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
hashFileContext = undefined

initializeLocally :: HashAlgorithm h => String -> FilePath -> IO (Provider h)
initializeLocally n fp = do
    let conf = Local.makeConfContext [2] OutputBase32 fp
    Local.initialize conf
    return $ Provider n ReadWrite (ProviderLocal conf)

existsIn :: HashAlgorithm h => ProviderBackend h -> Digest h -> IO Bool
existsIn (ProviderLocal conf) digest = Local.exists conf digest
existsIn (ProviderRemote _)   _      = return False

-- | Find if a provider provides the data related to this digest
find :: HashAlgorithm h => Providers h -> Digest h -> IO (Maybe (Provider h))
find providers digest = loop providers
  where
    loop []     = return Nothing
    loop (x:xs) = do
        r <- existsIn (providerBackend x) digest
        if r then return (Just x) else loop xs

-- | Find all the providers that provides the data related to this digest
findAll :: HashAlgorithm h => Providers h -> Digest h -> IO (Providers h)
findAll providers digest = loop [] providers
  where
    loop acc []     = return $ reverse acc
    loop acc (x:xs) = do
        r <- existsIn (providerBackend x) digest
        loop (if r then x : acc else acc) xs

-- | Check if a data related to a digest exists in one of the providers
exists :: HashAlgorithm h => Providers h -> Digest h -> IO Bool
exists providers digest = maybe False (const True) <$> find providers digest

-- | Import a file into a provider and return the digest
importInto :: HashAlgorithm h => Provider h -> Local.ImportType -> FilePath -> IO (Digest h)
importInto provider importType filePath =
    case providerBackend provider of
        ProviderLocal local -> Local.importFile local importType filePath
        ProviderRemote _    -> error "remote import not implemented"

-- | Import a file into a provider with a specific digest value
importIntoAt :: HashAlgorithm h => Provider h -> Local.ImportType -> Digest h -> FilePath -> IO ()
importIntoAt = error "importIntoAt not implemented"

-- | Push from @sourceProvider@ to @destProvider, the data associated with @digest@
pushFromTo :: HashAlgorithm h
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
