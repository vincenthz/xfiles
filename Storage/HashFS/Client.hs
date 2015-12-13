-- remote native
module Storage.HashFS.Client
    ( ConnectionConfig(..)
    , Client
    , EntityId
    , clientNew
    , clientEstablish
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
               -> IO ProtocolStatus
clientSendData conn digest fileSize dataReader =
    withClient conn $ \lsp -> do
        sendCommand lsp (Command fileSize $ Put digest)
        r <- expectAck <$> waitAck lsp
        case r of
            Left err -> return $ ProtocolErr err
            Right () -> do
                sendDataReader lsp dataReader
                return ProtocolOK

clientRecvDataDigest :: HashAlgorithm h
                     => Client
                     -> EntityId
                     -> Digest h -- ^ expected digest
                     -> DataWriterDigest h
                     -> IO ProtocolStatus
clientRecvDataDigest conn entityId expectedDigest (DataWriterDigest wcb fcb) =
    withClient conn $ \lsp -> do
        sendCommand lsp (Command 0 $ Get entityId)
        r <- expectAckSize <$> waitAck lsp
        case r of
            Left err            -> return $ ProtocolErr err
            Right payloadLength -> do
                computedDigest <- recvSizeDigest lsp payloadLength wcb
                fcb $ if computedDigest == expectedDigest then Just expectedDigest else Nothing
                return ProtocolOK

expectAck :: Ack -> Either ProtocolError ()
expectAck (AckOk)      = Right ()
expectAck (AckErr err) = Left err
expectAck (AckSize _)  = Left Unexpected

expectAckSize :: Ack -> Either ProtocolError Word64
expectAckSize (AckSize sz) = Right sz
expectAckSize (AckErr err) = Left err
expectAckSize (AckOk)      = Left Unexpected

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

clientEstablish :: Client -> IO ()
clientEstablish (Client cfg mmest) = do
    modifyMVar mmest $ \mest ->
        case mest of
            Nothing -> do
                establishedParams@(_, lsp) <- initiateConnection cfg
                return (Just establishedParams, ())
            Just _  -> do
                return (mest, ())

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
