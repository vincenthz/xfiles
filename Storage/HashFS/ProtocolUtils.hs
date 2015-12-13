module Storage.HashFS.ProtocolUtils where

import           Control.Monad

import qualified Data.ByteArray.Pack as C
import qualified Data.ByteArray.Parse as P
import           Data.ByteString (ByteString)
import           Data.Memory.Endian
import           Data.Word

import           Network.Socket hiding (send, recv)
import           Network.LSP
import qualified Data.ByteString as B
import qualified Control.Exception as E
import           Data.Typeable

import           Storage.HashFS.Types
import           Storage.HashFS.Hasher
import           Storage.HashFS.IO
import           Storage.HashFS.Protocol

import           Storage.Utils

sendDataReader :: LSP
               -> DataReader
               -> IO ()
sendDataReader lsp (DataReader rcb) = loop
  where
    loop = do
        mbs <- rcb
        case mbs of
            Nothing -> return ()
            Just bs -> send lsp bs >> loop

recvToDataWriter :: LSP
                 -> FileSize
                 -> DataWriter
                 -> IO ()
recvToDataWriter lsp fs (DataWriter pushWriter closeWriter) = loop fs
  where
    loop n
        | n == 0    = closeWriter >> return ()
        | otherwise = do
            let sz = fromIntegral $ min 2048 n
            b <- recv lsp sz
            when (B.length b > 0) $ pushWriter b
            loop (n - fromIntegral (B.length b))

-- | Receive data of a specific size and compute a digest
-- as the data is received. the user need to supply a function
-- that allow data to be written as received.
recvSizeDigest :: HashAlgorithm h
               => LSP                   -- ^ lsp connection
               -> PayloadLength         -- ^ size of the data
               -> (ByteString -> IO ()) -- ^ data writing callback
               -> IO (Digest h)         -- ^ digest of the data received
recvSizeDigest lsp payloadLength wcb =
    loop payloadLength hashInitializeContext
  where
    loop 0 hashCtx = return $! hashFinalize hashCtx
    loop n hashCtx = do
        let chunkLength = min (fromIntegral n) 2048
        v <- recv lsp chunkLength
        wcb v
        loop (n - fromIntegral (B.length v)) (hashUpdate hashCtx v)
