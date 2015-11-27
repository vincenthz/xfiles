-- remote native
module Storage.HashFS.Client
    ( ConnectionConfig(..)
    , Client
    , EntityId
    , clientNew
    , clientShut
    , clientSendData
    , clientRecvDataDigest
    ) where

import           Control.Concurrent.MVar

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
import           Storage.HashFS.ProtocolUtils

data ConnectionConfig = ConnectionConfig
    { concfgHostname    :: String
    , concfgPort        :: Int
    , concfgExpectedKey :: NodePublicKey
    , concfgMyKey       :: (NodeSecretKey, NodePublicKey)
    }

data Client = Client ConnectionConfig (MVar (Maybe (Socket, LSP)))

clientSendData :: Client
               -> EntityId
               -> PayloadLength
               -> DataReader
               -> IO ()
clientSendData conn digest fileSize dataReader =
    withClient conn $ \lsp -> do
        sendCommand lsp (Command fileSize $ Put digest)
        sendDataReader lsp dataReader

clientRecvDataDigest :: HashAlgorithm h
                     => Client
                     -> EntityId
                     -> Digest h -- ^ expected digest
                     -> DataWriterDigest h
                     -> IO ()
clientRecvDataDigest conn entityId expectedDigest (DataWriterDigest wcb fcb) =
    withClient conn $ \lsp -> do
        sendCommand lsp (Command 0 $ Get entityId)
        payloadLength  <- parsePayloadLength <$> recv lsp 8
        computedDigest <- recvSizeDigest lsp payloadLength wcb
        fcb $ if computedDigest == expectedDigest then Just expectedDigest else Nothing
        return ()

clientNew :: ConnectionConfig -> IO Client
clientNew concfg = Client <$> pure concfg <*> newMVar Nothing

withClient :: Client -> (LSP -> IO a) -> IO a
withClient (Client cfg mmest) f =
    modifyMVar mmest $ \mest ->
        case mest of
            Nothing -> do
                establishedParams@(_, lsp) <- initiateConnection cfg
                a <- f lsp
                return (Just establishedParams, a)
            Just (_, lsp) -> do
                a <- f lsp
                return (mest, a)

clientShut :: Client -> IO ()
clientShut (Client _ mmest) =
    modifyMVar mmest $ \mest ->
        case mest of
            Nothing        -> return (Nothing, ())
            Just (sock, _) -> do
                -- FIXME shutdown lsp too ?
                shutdown sock ShutdownBoth
                return (Nothing, ())

initiateConnection :: ConnectionConfig -> IO (Socket, LSP)
initiateConnection (ConnectionConfig host port expectedKey myKey) = do
    addr <- inet_addr host
    sock <- socket AF_INET Stream 0
    let sockaddr = SockAddrInet (fromIntegral port) addr
    connect sock sockaddr
    lsp <- client sock $ Config { private = myKey
                                , allowed = [expectedKey]
                                }
    return (sock, lsp)