{-# LANGUAGE MultiWayIf #-}
module Storage.HashFS.Protocol
    ( EntityId
    , PayloadLength
    , Command(..)
    , Ack(..)
    , Verb(..)
    , ProtocolStatus(..)
    , ProtocolError(..)
    , Arr(..)
    , Serializable(..)
    , sendCommand
    , waitCommand
    , sendAck
    , waitAck
    , parsePayloadLength
    -- * Exceptions
    , CommandInvalid
    , PayloadLengthInvalid
    ) where

import qualified Data.ByteArray.Pack as C
import qualified Data.ByteArray.Parse as P
import           Data.ByteString (ByteString)
import           Data.Memory.Endian
import           Data.Word
import           Data.Bits
import           Network.LSP

import qualified Data.ByteString as B
import qualified Control.Exception as E
import           Data.Typeable

import           Storage.HashFS.Types
import           Storage.HashFS.Hasher
import           Storage.HashFS.IO

-- | Any Entity
type EntityId = ByteString

type PayloadLength = Word64

data Command = Command PayloadLength Verb
    deriving (Show,Eq)

data Verb =
      Enumerate ByteString
    | Put       EntityId
    | Get       EntityId
    | Exist     EntityId
    | Check     EntityId
    | Create
    deriving (Show,Eq)

data Ack =
      AckOk
    | AckSize Word64
    | AckErr ProtocolError
    deriving (Show,Eq)

data ProtocolError =
      EntityNotFound
    | AlreadyExist
    | Unexpected
    | Unsupported
    deriving (Show,Eq,Ord,Enum,Bounded)

data ProtocolStatus =
      ProtocolOK
    | ProtocolErr ProtocolError
    deriving (Show,Eq)

data Arr a = Arr
    { arrLength :: Int -- of element
    , arrToList :: [a]
    } deriving (Show,Eq)

--class SerializableConstSize a where
--    toWireSize :: a -> Int

class Serializable a where
    toWire     :: a -> C.Packer ()
    fromWire   :: P.Parser ByteString a

instance Serializable Command where
    toWire   = putCommand
    fromWire = getCommand


putCommand :: Command -> C.Packer ()
putCommand (Command payload verb) = do
    C.putStorable (toLE payload)
    case verb of
        Enumerate bs -> C.putWord8 1 >> C.putBytes bs
        Put eid      -> C.putWord8 2 >> putEntityId eid
        Get eid      -> C.putWord8 3 >> putEntityId eid
        Exist eid    -> C.putWord8 4 >> putEntityId eid
        Check eid    -> C.putWord8 5 >> putEntityId eid
        Create       -> C.putWord8 6
  where
    putEntityId e = C.putBytes e >> C.fillUpWith (0 :: Word8)

getPayloadLength :: P.Parser ByteString PayloadLength
getPayloadLength = fromLE <$> P.takeStorable

getCommand :: P.Parser ByteString Command
getCommand = do
    payload <- getPayloadLength
    verbTy  <- P.anyByte
    verb    <- case verbTy of
                    1 -> Enumerate <$> P.takeWhile (/= 0)
                    2 -> Put <$> takeEntity
                    3 -> Get <$> takeEntity
                    4 -> Exist <$> takeEntity
                    5 -> Check <$> takeEntity
                    6 -> pure Create
                    _ -> fail ("invalid type: " ++ show verbTy)
    return $ Command payload verb
  where
    takeEntity = P.takeWhile (/= 0)

putAck :: Ack -> C.Packer ()
putAck a = do
    let v = case a of
                AckOk       -> 0
                AckErr perr -> putTogether 1 (fromIntegral $ fromEnum perr)
                AckSize sz  -> putTogether 2 sz
    C.putStorable $ toLE v
  where
    putTogether :: Word8 -> Word64 -> Word64
    putTogether cmd dat
        | cmd == 0         = error "cannot build ack: cmd is 0"
        | dat >= dataLimit = error "cannot build ack: data too big"
        | otherwise        = (fromIntegral cmd `shiftL` 56) .|. dat

getAck :: P.Parser ByteString Ack
getAck = do
    v <- fromLE <$> P.takeStorable
    if v == 0
        then return AckOk
        else parse v
  where
    mb :: Word64
    mb = fromIntegral $ fromEnum (maxBound :: ProtocolError)

    parse v
        | cmd == 1  =
            if | dat > mb  -> fail ("invalid AckErr: " ++ show dat)
               | otherwise -> return $ AckErr $ toEnum $ fromIntegral dat
        | cmd == 2  = return $ AckSize dat
        | otherwise = fail ("invalid Ack: unknown cmd: " ++ show cmd ++ " dat: " ++ show dat)
      where
        cmd :: Word8
        cmd = fromIntegral (v `shiftR` 56)

        dat :: Word64
        dat = v .&. (dataLimit - 1)

dataLimit :: Word64
dataLimit = 1 `shiftL` 56

data CommandInvalid = CommandInvalid
    deriving (Show,Eq,Typeable)

instance E.Exception CommandInvalid

data AckInvalid = AckInvalid
    deriving (Show,Eq,Typeable)

instance E.Exception AckInvalid

data PayloadLengthInvalid = PayloadLengthInvalid
    deriving (Show,Eq,Typeable)

instance E.Exception PayloadLengthInvalid

commandSize :: Int
commandSize = 128

ackSize :: Int
ackSize = 8

parsePayloadLength :: ByteString -> PayloadLength
parsePayloadLength bs
    | B.length bs /= 8 = E.throw PayloadLengthInvalid
    | otherwise        =
        case P.parse getPayloadLength bs of
            P.ParseOK _ p -> p
            _             -> E.throw PayloadLengthInvalid

waitCommand :: LSP -> IO Command
waitCommand lsp = do
    bs <- recv lsp commandSize
    case P.parse getCommand bs of
        P.ParseOK _ cmd -> return cmd
        _               -> E.throwIO CommandInvalid

waitAck :: LSP -> IO Ack
waitAck lsp = do
    bs <- recv lsp ackSize
    case P.parse getAck bs of
        P.ParseOK _ cmd -> return cmd
        _               -> E.throwIO AckInvalid

--waitArr :: ConstantSize a => LSP -> IO (Arr a)
--waitArr lsp = do
--    bs <- recv lsp
--waitArrOf :: LSP -> IO (Arr a)

sendCommand :: LSP -> Command -> IO ()
sendCommand lsp cmd = do
    send lsp (either error id $ C.fill commandSize $ putCommand cmd)

sendAck :: LSP -> Ack -> IO ()
sendAck lsp ack = do
    send lsp (either error id $ C.fill ackSize $ putAck ack)
