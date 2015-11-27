{-# LANGUAGE OverloadedStrings #-}
module Network.LSP
    ( Config(..)
    , Backend(..)
    , NodeSecretKey
    , NodePublicKey
    , LSP
    , client
    , server
    , send
    , recvPacket
    , recv
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Concurrent.MVar
import           Control.Concurrent.Async
import qualified Control.Exception as E

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

import           Network.Socket (Socket)
import qualified Network.Socket.ByteString as N

import           Network.LSP.Crypto
import           Network.LSP.Context
import           Network.LSP.Exception
import           Network.LSP.Record
import           Network.LSP.IO
import           Network.LSP.Wire
import           Network.LSP.Types

newtype Remote a = Remote a

newtype Local a = Local a

data Side = Client | Server
    deriving (Show,Eq)

-- | Create a new client LSP connection
client :: Backend backend
       => backend
       -> Config
       -> IO LSP
client backend cfg = do
    (dh, localHello) <- newHello (private cfg)
    sendHello backend localHello
    remoteHello <- recvHello (allowed cfg) backend
    st          <- computeState Client dh localHello remoteHello
    rbuf        <- newRBuf
    return $ LSP { lspSocket = B backend
                 , lspConfig = cfg
                 , lspState  = st
                 , lspRBuf   = rbuf
                 }

-- | Create a new server LSP connection
server :: Backend backend
       => backend
       -> Config
       -> IO LSP
server backend cfg = do
    remoteHello      <- recvHello (allowed cfg) backend
    (dh, localHello) <- newHello (private cfg)
    (st, ())         <- concurrently
                            (computeState Server dh localHello remoteHello)
                            (sendHello backend localHello)
    rbuf             <- newRBuf
    return $ LSP { lspSocket = B backend
                 , lspConfig = cfg
                 , lspState  = st
                 , lspRBuf   = rbuf
                 }

-- | Send data through a LSP connection
-- 4096 bytes (- metadata needed) at a time
send :: LSP -> ByteString -> IO ()
send lsp inp
    | B.null inp = return ()
    | otherwise  = do
        let (plainText,r) = B.splitAt (maxRecordSize-metaDataSize) inp
        dat <- doEncrypt plainText
        withBackend lsp $ \b -> sendData b dat
        send lsp r
  where
    doEncrypt p = do
        (seqNum, seqIv) <- getTX lsp
        let hdr    = B.pack [0,0,c,d]
            seqHdr = putSequenceNumber seqNum
            key    = keyTx $ lspState lsp
        return $ B.concat [hdr, encrypt key seqIv (hdr `B.append` seqHdr) p]
      where
        len = B.length p + metaDataSize
        d   = fromIntegral (len `mod` 256)
        c   = fromIntegral (len `div` 256)

-- | Receive data through a LSP connection
--
-- Receive the header data from the backend,
-- and then get the payload, which is decrypted
--
-- Empty bytestring are valid packet, and do not
-- indicate end of input (can be used for keepalive for example)
recvPacket :: LSP -> IO ByteString
recvPacket lsp = do
    (hdr, datLen) <- withBackend lsp recvDataHeader
    when (datLen < metaDataSize) $ E.throwIO RecvBadRecord
    payload       <- withBackend lsp $ \b -> backendRecv b (datLen `mod` maxRecordSize)
    plaintext     <- doDecrypt hdr payload
    maybe (E.throwIO RecvBadRecord) return plaintext
  where
    doDecrypt hdr dat = do
        (seqNum, seqIv) <- getRX lsp
        let seqHdr = putSequenceNumber seqNum
            key    = keyRx $ lspState lsp
        return (decrypt key seqIv (hdr `B.append` seqHdr) dat)

recv :: LSP -> Int -> IO ByteString
recv lsp nbBytes = do
    -- check if we have enough already from the buffer
    ini <- modifyRBuf (\buf -> let (b1, b2) = B.splitAt nbBytes buf in return (b2, b1))
    if B.length ini == nbBytes
        then return ini
        else -- need to peek into new packets from the backend
            B.concat . ((:) ini) <$> loop (nbBytes - B.length ini)
  where
    modifyRBuf f = modifyMVar (lspRBuf lsp) f
    loop n
        | n == 0    = return []
        | otherwise = do
            p <- recvPacket lsp
            case compare (B.length p) n of
                EQ -> return [p]
                LT -> ((:) p) <$> loop (n - B.length p)
                GT -> do
                    let (b1,b2) = B.splitAt n p
                    modifyRBuf (\buf -> return (buf `B.append` b2, ()))
                    return [b1]

newHello :: (NodeSecretKey, NodePublicKey)
         -> IO (SessionPublicKey -> SessionSharedKey, Local Hello)
newHello nodeKey = do
    let version = Version 1
    (ran, dh, pub) <- generateNewSession
    let signature = computeSignature nodeKey (putHelloSignature ran version pub (snd nodeKey))
        hello     = Hello ran version $ HelloDataV1 pub (snd nodeKey) signature
    return (dh, Local hello)

sendHello :: Backend backend => backend -> Local Hello -> IO ()
sendHello socket (Local h) =
    backendSend socket (putHello h)

recvHello :: Backend backend => WhiteList -> backend -> IO (Remote Hello)
recvHello whitelist socket = do
    hello <- either error id . parseHello <$> recvExact socket helloSize
    case helloGetData hello of
        HelloDataV1 pub nodeKey nodeSig -> do
            checkWhiteList whitelist nodeKey
            let helloSigData = putHelloSignature (helloGetRandom hello) (helloGetVersion hello) pub nodeKey
                verified     = verifySignature nodeKey nodeSig helloSigData
            unless verified $ E.throwIO BadHello
            return $ Remote hello

checkWhiteList :: WhiteList -> NodePublicKey -> IO ()
checkWhiteList whitelist pub =
    unless (pub `elem` whitelist) $ E.throwIO $ UnknownNode pub

computeState :: Side
             -> (SessionPublicKey -> DhSecret)
             -> Local Hello
             -> Remote Hello
             -> IO State
computeState side dh (Local localHello) (Remote remoteHello) = do
    case helloGetData remoteHello of
        HelloDataV1 remotePub _ _ -> do
            let shared = dh remotePub
                master  = generateMaster (helloGetRandom $ if side == Client then localHello else remoteHello)
                                         (helloGetRandom $ if side == Client then remoteHello else localHello)
                                         shared
                (key1, key2, iv1, iv2) = generateCryptoState master

            txSeq <- newSequence $ if side == Client then iv1 else iv2
            rxSeq <- newSequence $ if side == Client then iv2 else iv1
            let st = State { keyTx = if side == Client then key1 else key2
                           , keyRx = if side == Client then key2 else key1
                           , seqTx = txSeq
                           , seqRx = rxSeq
                           }
            return $ st

maxRecordSize :: Int
maxRecordSize = 4096

-- 16 bytes of metadata per packet: 16 bytes authtag
metaDataSize :: Int
metaDataSize = 16
