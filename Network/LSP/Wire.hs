module Network.LSP.Wire
    ( parseHello
    , putHello
    , putHelloSignature
    , putSequenceNumber
    , helloSize
    ) where

import           Network.LSP.Record
import           Network.LSP.Crypto
import           Network.LSP.Types
import           Control.Applicative
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteArray.Pack as C
import qualified Data.ByteArray.Parse as P
import           Data.Memory.Endian
import           Data.Word

parseHello :: ByteString -> Either String Hello
parseHello bs
    | B.length bs < 34 = Left "hello: expecting more data"
    | otherwise        =
        case P.parse parseCommon bs of
            P.ParseFail f -> Left f
            P.ParseMore _ -> Left "hello: expecting more data"
            P.ParseOK r hello
                | B.null r -> Right hello
                | otherwise -> Left "hello: unparsed data"
  where
    parseCommon = do
        -- common part
        ran <- SessionRandom    <$> P.take 32
        ver <- Version . fromLE <$> P.takeStorable
        dat <- parseSpecific ver
        return $ Hello ran ver dat

    parseSpecific :: Version -> P.Parser ByteString HelloData
    parseSpecific (Version 1) = HelloDataV1 <$> parseSessionPublicKey
                                            <*> parseNodePublicKey
                                            <*> parseNodeSignature
    parseSpecific i = fail ("invalid version " ++ show i)

putHello :: Hello -> ByteString
putHello (Hello ran (Version ver) (HelloDataV1 sessionPub nodePub nodeSig)) = either error id $ C.fill helloSize $ do
    C.putBytes ran
    C.putStorable (toLE ver)
    C.putBytes sessionPub
    C.putBytes nodePub
    C.putBytes nodeSig

putHelloSignature :: SessionRandom -> Version -> SessionPublicKey -> NodePublicKey -> ByteString
putHelloSignature ran (Version ver) sessionPub nodePub = either error id $ C.fill (helloSize - 64) $ do
    C.putBytes ran
    C.putStorable (toLE ver)
    C.putBytes sessionPub
    C.putBytes nodePub

putSequenceNumber :: Word64 -> ByteString
putSequenceNumber seqNum = either error id $ C.fill 8 $
    C.putStorable (toLE seqNum)

helloSize :: Int
helloSize = 32 + 2 + 32 + 32 + 64
