{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Network.LSP.Context
    ( Config(..)
    , Backend(..)
    , B(..)
    , LSP(..)
    , WhiteList
    , State(..)
    , Sequence
    , newSequence
    , getTX
    , getRX
    , withBackend
    ) where

import           Control.Concurrent.MVar
import           Network.Socket (Socket)
import qualified Network.Socket.ByteString as N
import           Network.LSP.Crypto
import           Data.Word
import           Data.ByteString (ByteString)
import qualified Data.ByteArray as B
import           Foreign.Ptr
import           Foreign.Storable

type WhiteList = [NodePublicKey]

-- | LSP configuration
data Config = Config
    { private :: (NodeSecretKey, NodePublicKey)
    , allowed :: WhiteList
    }

data State = State
    { seqTx    :: MVar Sequence
    , seqRx    :: MVar Sequence
    , keyTx    :: SymmetricKey
    , keyRx    :: SymmetricKey
    }

data Sequence = Sequence
    { seqNum :: !SequenceNumber
    , seqIv  :: !ByteString
    }

newtype SequenceNumber = SequenceNumber Word64

newSequence :: ByteString -> IO (MVar Sequence)
newSequence !iv = newMVar $! Sequence { seqNum = SequenceNumber 1, seqIv = iv }

incrementBs :: ByteString -> ByteString
incrementBs n = B.copyAndFreeze n $ \s ->
    loop s $ s `plusPtr` ((B.length n) - 1)
  where
      loop :: Ptr Word8 -> Ptr Word8 -> IO ()
      loop s p
          | s == p    = peek s >>= poke s . (+) 1
          | otherwise = do
              r <- (+) 1 <$> peek p
              poke p r
              if r == 0 then loop s (p `plusPtr` (-1)) else return ()

-- | LSP Context
data LSP = LSP
    { lspSocket :: B
    , lspConfig :: Config
    , lspState  :: State
    }

data B = forall b . Backend b => B b

withBackend :: LSP -> (forall backend . Backend backend => backend -> IO a) -> IO a
withBackend lsp f = case lspSocket lsp of
                        B back -> f back

class Backend a where
    backendSend :: a -> ByteString -> IO ()
    backendRecv :: a -> Int -> IO ByteString 

instance Backend Socket where
    backendSend = N.sendAll
    backendRecv = N.recv

incrState :: Sequence -> IO (Sequence, (Word64, ByteString))
incrState (Sequence (SequenceNumber r) iv) = do
    return (Sequence { seqNum = SequenceNumber (r+1)
                     , seqIv  = incrementBs iv
                     }
           , (r,iv))

getTX :: LSP -> IO (Word64, ByteString)
getTX lsp = modifyMVar (seqTx $ lspState lsp) incrState

getRX :: LSP -> IO (Word64, ByteString)
getRX lsp = modifyMVar (seqRx $ lspState lsp) incrState
