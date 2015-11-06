{-# LANGUAGE LambdaCase #-}
module Network.LSP.IO
    ( recvExact 
    , recvDataHeader
    , sendData
    ) where

import           Control.Applicative
import           Control.Monad
import qualified Control.Exception as E
import           Control.Concurrent.MVar

import           Data.Bits
import           Data.Typeable

import           Network.LSP.Context
import           Network.LSP.Crypto
import           Network.LSP.Exception
import           Network.LSP.Record

import           Network.Socket (Socket)
import qualified Network.Socket.ByteString as N

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

recvExact :: Backend backend => backend -> Int -> IO ByteString
recvExact socket nbBytes = loop [] nbBytes
  where loop acc n
            | n == 0    = return $ B.concat $ reverse acc
            | otherwise = do
                dat <- backendRecv socket n 
                when (B.length dat == 0) $ E.throw (RecvError n)
                loop (dat:acc) (n - B.length dat)

recvDataHeader :: Backend backend => backend -> IO (ByteString, Int)
recvDataHeader socket = do
    b <- recvExact socket 4
    case B.unpack b of
        [_,_,c,d] -> do
            let recordLength = (fromIntegral (c `shiftL` 8) .|. fromIntegral d)
            return (b, recordLength)
        _ -> error "invalid recvHdr wrong size"

sendData :: Backend a => a -> ByteString -> IO ()
sendData socket dat = backendSend socket dat
