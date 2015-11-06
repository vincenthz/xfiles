module Network.LSP.Record
    ( Hello(..)
    , HelloData(..)
    , Data(..)
    ) where

import           Data.Word
import           Data.Bits
import           Data.ByteString (ByteString)
import           Data.ByteArray (ByteArrayAccess)
import           Network.LSP.Crypto
import           Network.LSP.Types

------------------------------------------------------------------------

data Hello = Hello
    { helloGetRandom  :: SessionRandom
    , helloGetVersion :: Version
    , helloGetData    :: HelloData
    }
    deriving (Show,Eq)

data HelloData = HelloDataV1 SessionPublicKey NodePublicKey NodeSignature
    deriving (Show,Eq)

data Data = Data Word32 ByteString
    deriving (Show,Eq)
