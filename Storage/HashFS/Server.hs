module Storage.HashFS.Server
    ( Server
    , ServerImplStore(..)
    , ServerImplTrust(..)
    , serverNew
    , serverShut
    ) where

import           Control.Concurrent
import           Control.Concurrent.MVar
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
import           Storage.HashFS.ProtocolUtils
import           Storage.HashFS.Server.Handler
import           Storage.Utils (FileSize)

type ClientHandle = MVar LSP

type Server = Handler ClientHandle

serverNew :: ServerImplStore
          -> Socket
          -> IO Server
serverNew impl sock = do
    handler <- handlerStart (serverProcessConn impl)
    let listener = listenerCreate sock mempty onCreate
    handlerAddListener handler listener
    return handler
  where
    onCreate sock sockaddr = do
        let privKey    = undefined
            clientKeys = undefined
        let lspConf = Config { private = privKey
                             , allowed = clientKeys
                             }
        lsp <- server sock lspConf >>= newMVar
        let onClose = shutdown sock ShutdownBoth
        return $ Right (lsp, onClose)

serverShut :: Server -> IO ()
serverShut serv = do
    -- FIXME shutdown all listener, and all already on clients
    --shutdown (serverListeningSocket serv) ShutdownBoth
    return ()

-- | Server Store is just a naive implementation that does not check any property
-- of the entity id compared to the data. the entity id may or may not have
-- relation, and it's very likely this node act like a simple storage node
data ServerImplStore = ServerImplStore
    { serverImplGet :: EntityId -> IO (Either ProtocolError (FileSize, DataReader))
    , serverImplPut :: EntityId -> FileSize -> IO (Either ProtocolError DataWriter)
    }

-- | Server Store for trusted implementation. the trusted implementation has a knowledge
-- of the underlaying storing mechanism and the relation between the entity id and the
-- data content.
data ServerImplTrust h = ServerImplTrust
    { serverImplTrustGet :: Digest h -> IO (Either ProtocolError (FileSize, DataReader))
    , serverImplTrustPut :: Digest h -> FileSize -> IO (Either ProtocolError (DataWriterDigest h))
    }

serverProcessConn :: ServerImplStore -> ClientHandle -> IO ()
serverProcessConn impl serverConn = forever $ do
    -- FIXME cleanup connection here in case of exception or shutdown
    serverProcessCommand impl serverConn
    serverProcessConn impl serverConn


serverProcessCommand :: ServerImplStore -> ClientHandle -> IO ()
serverProcessCommand impl clientHandle = grab clientHandle $ \lsp -> do
    (Command payload verb) <- waitCommand lsp
    case verb of
        Get ent -> do
            r <- serverImplGet impl $ ent
            case r of
                Left err            -> sendAckErr lsp err
                Right (fsz, reader) -> sendAck lsp (AckSize fsz) >> sendDataReader lsp reader
        Put ent -> do
            r <- (serverImplPut impl) ent payload
            case r of
                Left err -> sendAckErr lsp err
                Right dw -> recvToDataWriter lsp payload dw
        _       -> sendAckErr lsp Unsupported

sendAckErr :: LSP -> ProtocolError -> IO ()
sendAckErr lsp perr = sendAck lsp (AckErr perr)
