-- remote native
module Storage.HashFS.Native
    where

import qualified Data.ByteArray.Pack as C
import qualified Data.ByteArray.Parse as P
import           Data.ByteString (ByteString)
import           Data.Memory.Endian
import           Data.Word

import           Network.Socket (Socket, bind, socket, connect, inet_addr, SockAddr(..))
import           Network.LSP
import qualified Data.ByteString as B
import qualified Control.Exception as E
import           Data.Typeable

data Connection = Connection String

type EntityId = ByteString

type PayloadLength = Word64

data Command = Command PayloadLength Verb

data Verb =
      Enumerate ByteString
    | Put       EntityId
    | Get       EntityId
    | Exist     EntityId
    | Create
    | Invalid
    deriving (Show,Eq)

putCommand (Command payload verb) = do
    C.putStorable (toLE payload)
    case verb of
        Enumerate bs -> C.putWord8 1 >> C.putBytes bs
        Put eid      -> C.putWord8 2 >> putEntityId eid
        Get eid      -> C.putWord8 3 >> putEntityId eid
        Exist eid    -> C.putWord8 4 >> putEntityId eid
        Create       -> C.putWord8 5
  where
    putEntityId e = C.putBytes e >> C.fillUpWith (0 :: Word8)

getCommand = do
    payload <- fromLE <$> P.takeStorable
    verbTy  <- P.anyByte
    verb    <- case verbTy of
                    1 -> Enumerate <$> P.takeWhile (/= 0)
                    2 -> Put <$> takeEntity
                    3 -> Get <$> takeEntity
                    4 -> Exist <$> takeEntity
                    5 -> pure Create
                    _ -> pure Invalid
    return $ Command payload verb
  where
    takeEntity = P.takeWhile (/= 0)

data CommandInvalid = CommandInvalid
    deriving (Show,Eq,Typeable)

instance E.Exception CommandInvalid

commandSize :: Int
commandSize = 128

waitCommand :: LSP -> IO (Verb, PayloadLength)
waitCommand lsp = do
    bs <- recv lsp commandSize
    (verb, payload) <- case P.parse getCommand bs of
                            P.ParseOK _ (Command p v) -> return (v, p)
                            _                         -> E.throwIO CommandInvalid
    case verb of
        Invalid -> E.throwIO CommandInvalid
        _       -> return (verb, payload)
{-
        -- good
        Enumerate prefix -> undefined
        Get e            -> undefined
        Put e            -> undefined
        Exist e          -> undefined
        Create           -> undefined
-}

sendCommand :: LSP -> Command -> IO ()
sendCommand lsp cmd = do
    send lsp (either error id $ C.fill commandSize $ putCommand cmd)

sendData :: Connection
         -> EntityId
         -> FileSize
         -> DataReader
         -> IO ()
sendData conn digest fileSize (DataReader rcb) = do
    withConnection conn $ \lsp -> do
        sendCommand lsp (Command fileSize $ Put digest)
        loop lsp
  where
    loop lsp = do
        mbs <- rcb
        case mbs of
            Nothing -> return ()
            Just bs -> send lsp bs >> loop lsp

recvData :: Connection
         -> EntityId
         -> FileSize
         -> IO ()
recvData conn digest = do
    withConnection conn $ \lsp -> do
        sendCommand lsp (Command fileSize $ Get digest)
        -- FIXME
        return ()

withConnection :: Connection -> (Socket -> LSP -> IO a) -> IO a
withConnection con f = do
    (sock, lsp) <- initiateConnection
    f lsp
    shutdown sock

initiateConnection :: Connection -> IO (Socket, LSP)
initiateConnection (Connection host port expectedKey myKey) = do
    addr <- inet_addr host
    sock <- socket AF_INET Stream 0
    let sockaddr = SockAddrInet (fromIntegral port) addr
    connect sock sockaddr
    lsp <- client connect $ Config { private = myKey
                                   , allowed = [expectedKey]
                                   }
    return (sock, lsp)
