module Storage.HashFS.Hasher
    ( Hasher
    , HashAlgorithm
    , Context
    , Digest
    , hasherInit
    , hasherInitContext
    , hashGetDummy
    , hashCompute
    , hashInitialize
    , hashInitializeContext
    , hashUpdate
    , hashFinalize
    , hashFile
    , digestFromByteString
    -- * Specific algorithm
    , SHA256(..)
    , SHA512(..)
    ) where

import           Crypto.Hash
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L

newtype Hasher h = Hasher h
    deriving (Eq)

instance Show (Hasher h) where
    show _ = "hasher"

hasherInit :: HashAlgorithm h => h -> Hasher h
hasherInit = Hasher

-- the algorithm is dummy, so this should be safe.
hasherInitContext :: HashAlgorithm h => Hasher h
hasherInitContext = Hasher (error "internal error: hasherInitContext has been evaluated but shouldn't have")

hashGetDummy :: HashAlgorithm h => Hasher h -> Digest h
hashGetDummy (Hasher h) = hashFinalize $ hashInitWith h

hashCompute :: HashAlgorithm h => Hasher h -> [ByteString] -> Digest h
hashCompute (Hasher h) = hashFinalize . hashUpdates (hashInitWith h)

hashInitialize :: HashAlgorithm h => Hasher h -> Context h
hashInitialize (Hasher h) = hashInitWith h

hashInitializeContext :: HashAlgorithm h => Context h
hashInitializeContext = hashInit

hashFile :: HashAlgorithm h => Hasher h -> FilePath -> IO (Digest h)
hashFile _ filePath = hashlazy <$> L.readFile filePath
