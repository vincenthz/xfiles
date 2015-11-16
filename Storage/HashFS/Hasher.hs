module Storage.HashFS.Hasher
    ( Hasher
    , HashAlgorithm
    , Context
    , Digest
    , hasherInit
    , hashGetDummy
    , hashCompute
    , hashInitialize
    , hashUpdate
    , hashFinalize
    , digestFromByteString
    -- * Specific algorithm
    , SHA256(..)
    , SHA512(..)
    ) where

import           Crypto.Hash
import           Data.ByteString (ByteString)

newtype Hasher h = Hasher h

hasherInit :: HashAlgorithm h => h -> Hasher h
hasherInit = Hasher

hashGetDummy :: HashAlgorithm h => Hasher h -> Digest h
hashGetDummy (Hasher h) = hashFinalize $ hashInitWith h

hashCompute :: HashAlgorithm h => Hasher h -> [ByteString] -> Digest h
hashCompute (Hasher h) = hashFinalize . hashUpdates (hashInitWith h)

hashInitialize :: HashAlgorithm h => Hasher h -> Context h
hashInitialize (Hasher h) = hashInitWith h
