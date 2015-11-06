module Network.LSP.Exception
    ( RecvError(..)
    , RecvBadRecord(..)
    , BadHello(..)
    , UnknownNode(..)
    ) where

import           Data.Typeable
import qualified Control.Exception as E
import           Network.LSP.Crypto

data RecvBadRecord = RecvBadRecord
    deriving (Show,Eq,Typeable)

instance E.Exception RecvBadRecord

data RecvError = RecvError Int
    deriving (Show,Eq,Typeable)

instance E.Exception RecvError

data BadHello = BadHello
    deriving (Show,Eq,Typeable)

instance E.Exception BadHello

data UnknownNode = UnknownNode NodePublicKey
    deriving (Show,Eq,Typeable)

instance E.Exception UnknownNode
