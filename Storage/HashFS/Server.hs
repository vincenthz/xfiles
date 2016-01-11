module Storage.HashFS.Server
    ( Server
    , ServerImplStore(..)
    , ServerImplTrust(..)
    , serverNew
    , serverListen
    , serverWait
    , serverShut
    , serverSetLog
    ) where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Concurrent.Chan
import           Control.Concurrent.Async
import           Control.Monad

import qualified Data.ByteArray.Pack as C
import qualified Data.ByteArray.Parse as P
import           Data.ByteString (ByteString)
import           Data.Memory.Endian
import           Data.Word
import           Data.IORef

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
import           Storage.HashFS.Server.Log
import           Storage.Utils (FileSize)

type ClientHandle = MVar LSP

data TaskRet =
      ListeningThreadEnd
    | Notified
    deriving (Show,Eq)

data Server = Server
    { serverHandler :: Handler ClientHandle
    , serverLog     :: IORef (LogEvent -> IO ())
    , serverTasks   :: IORef [Async ()]
    }

type KeyByAddr = SockAddr -> ((NodeSecretKey, NodePublicKey), [NodePublicKey])

doLog :: IORef (LogEvent -> IO ()) -> LogEvent -> IO ()
doLog r le = readIORef r >>= \f -> f le

serverNew :: ServerImplStore
          -> IO Server
serverNew impl = do
    logRef  <- newIORef (\_ -> return ())
    handler <- handlerCreate (serverProcessConn impl)
    Server <$> pure handler <*> pure logRef <*> newIORef []

serverListen :: Socket
             -> KeyByAddr
             -> Server
             -> IO ()
serverListen listeningSocket keyByAddr srv = do
    let listener = listenerCreate listeningSocket mempty onCreate
    a <- handlerAddListener (serverHandler srv) listener
    modifyIORef (serverTasks srv) $ \t -> (a : t)
  where
    onCreate sock sockaddr = do
        doLog (serverLog srv) (ClientConnected sockaddr)
        let (privKey, clientKeys) = keyByAddr sockaddr
            lspConf = Config { private = privKey
                             , allowed = clientKeys
                             }
        lsp <- server sock lspConf >>= newMVar
        let onClose = shutdown sock ShutdownBoth
        doLog (serverLog srv) (ClientEstablished sockaddr)
        return $ Right (lsp, onClose)

serverNotify :: Server -> IO ()
serverNotify _ =
    return ()

serverWait :: Server -> IO ()
serverWait srv = forever $ do
    tasks <- readIORef (serverTasks srv)
    _     <- waitAny tasks
    return ()

serverSetLog :: Server
             -> (LogEvent -> IO ())
             -> IO ()
serverSetLog s logCb = writeIORef (serverLog s) logCb

serverShut :: Server -> IO ()
serverShut _ = do
    -- FIXME shutdown all listener, and all already on clients
    --shutdown (serverListeningSocket serv) ShutdownBoth
    --writeChan ch ()
    return ()

type ProtocolReturn a = IO (Either ProtocolError a)

-- | Server Store is just a naive implementation that does not check any property
-- of the entity id compared to the data. the entity id may or may not have
-- relation, and it's very likely this node act like a simple storage node
data ServerImplStore = ServerImplStore
    { serverImplGet :: EntityId -> ProtocolReturn (FileSize, DataReader)
    , serverImplPut :: EntityId -> FileSize -> ProtocolReturn DataWriter
    , serverImplEnum :: ByteString -> ProtocolReturn (Arr EntityId)
    , serverImplInfo :: Arr EntityId -> ProtocolReturn (Arr FileSize)
    }

-- | Server Store for trusted implementation. the trusted implementation has a knowledge
-- of the underlaying storing mechanism and the relation between the entity id and the
-- data content.
data ServerImplTrust h = ServerImplTrust
    { serverImplTrustGet :: Digest h -> IO (Either ProtocolError (FileSize, DataReader))
    , serverImplTrustPut :: Digest h -> FileSize -> IO (Either ProtocolError (DataWriterDigest h))
    }

serverProcessConn :: ServerImplStore -> ClientHandle -> IO ()
serverProcessConn impl serverConn = do
    -- FIXME cleanup connection here in case of exception or shutdown
    serverProcessCommand impl serverConn
    serverProcessConn impl serverConn


serverProcessCommand :: ServerImplStore
                     -> ClientHandle
                     -> IO ()
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
{-
        Enum prefix -> do
            r <- (serverImplEnum impl) prefix
            case r of
                Left err   -> sendAckErr lsp err
                Right ents -> do
                    sendAck lsp (AckSize 0)
        Info elems -> do
            sendAck lsp (AckOk)
-}
        _       -> sendAckErr lsp Unsupported
  where

sendAckErr :: LSP -> ProtocolError -> IO ()
sendAckErr lsp perr = sendAck lsp (AckErr perr)
