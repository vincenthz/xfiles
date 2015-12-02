{-# LANGUAGE BangPatterns #-}
module Storage.HashFS.IO
    ( DataReader(..)
    , DataWriter(..)
    , DataWriterDigest(..)
    , filepathDataReader
    , filepathDataWriter
    , onDataChunksDigest
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           System.IO
import           System.Directory
import           Storage.HashFS.Hasher

newtype DataReader = DataReader (IO (Maybe ByteString))

data DataWriter = DataWriter (ByteString -> IO ()) (IO ())

data DataWriterDigest h = DataWriterDigest (ByteString -> IO ())
                                           (Maybe (Digest h) -> IO ())

filepathDataReader :: FilePath -> IO DataReader
filepathDataReader fp = do
    h <- openBinaryFile fp ReadMode
    let rC = do
            d <- B.hGet h 4096
            if B.null d
                then hClose h >> return Nothing
                else return $ Just d
    return $ DataReader rC

filepathDataWriter :: FilePath -> IO DataWriter
filepathDataWriter fp = do
    e <- doesFileExist fp
    if e
        then error "file already exist"
        else do
            h <- openBinaryFile fp WriteMode
            return $ DataWriter (B.hPut h) (hClose h)

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

