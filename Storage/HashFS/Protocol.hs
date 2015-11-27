module Storage.HashFS.Protocol
    ( EntityId
    , PayloadLength
    , Command(..)
    , Verb(..)
    , sendCommand
    , waitCommand
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

data Verb =
      Enumerate ByteString
    | Put       EntityId
    | Get       EntityId
    | Exist     EntityId
    | Create
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

getPayloadLength :: P.Parser ByteString PayloadLength
getPayloadLength = fromLE <$> P.takeStorable

getCommand = do
    payload <- getPayloadLength
    verbTy  <- P.anyByte
    verb    <- case verbTy of
                    1 -> Enumerate <$> P.takeWhile (/= 0)
                    2 -> Put <$> takeEntity
                    3 -> Get <$> takeEntity
                    4 -> Exist <$> takeEntity
                    5 -> pure Create
                    _ -> fail ("invalid type: " ++ show verbTy)
    return $ Command payload verb
  where
    takeEntity = P.takeWhile (/= 0)

data CommandInvalid = CommandInvalid
    deriving (Show,Eq,Typeable)

instance E.Exception CommandInvalid

data PayloadLengthInvalid = PayloadLengthInvalid
    deriving (Show,Eq,Typeable)

instance E.Exception PayloadLengthInvalid


commandSize :: Int
commandSize = 128

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

sendCommand :: LSP -> Command -> IO ()
sendCommand lsp cmd = do
    send lsp (either error id $ C.fill commandSize $ putCommand cmd)

