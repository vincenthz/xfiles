{-# LANGUAGE BangPatterns #-}
module Storage.HashFS.IO
    ( DataReader(..)
    , DataWriter(..)
    , DataWriterDigest(..)
    , filepathDataReader
    , onDataChunksDigest
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           System.IO
import           Storage.HashFS.Hasher

newtype DataReader = DataReader (IO (Maybe ByteString))

data DataWriter = DataWriter (ByteString -> IO ()) (IO ())

data DataWriterDigest h = DataWriterDigest (ByteString -> IO ())
                                           (Maybe (Digest h) -> IO ())

filepathDataReader :: FilePath -> IO DataReader
filepathDataReader fp = do
    h <- openFile fp ReadMode
    let rC = do
            d <- B.hGet h 4096
            if B.null d
                then hClose h >> return Nothing
                else return $ Just d
    return $ DataReader rC

onDataChunksDigest :: HashAlgorithm h
                   => DataReader         -- ^ read data callback
                   -> DataWriterDigest h -- ^ write data callback
                   -> IO (Digest h)
onDataChunksDigest (DataReader readCallback) (DataWriterDigest writeCallback writeDone) =
    loop hashInitializeContext
  where
    loop !ctx = do
        mb <- readCallback
        case mb of
            Nothing -> do
                let !digest = hashFinalize ctx
                writeDone (Just digest)
                return $ digest
            Just b  -> do
                writeCallback b
                loop (hashUpdate ctx b)

