{-# LANGUAGE BangPatterns #-}
module Storage.HashFS.IO
    ( DataReader(..)
    , DataWriter(..)
    , DataWriterDigest(..)
    , filepathDataReader
    , filepathDataWriter
    , pipeData
    , pipeDataDigest
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           System.IO
import           System.Directory
import           Storage.HashFS.Hasher

data DataReader = DataReader
    { dataReaderRead  :: IO (Maybe ByteString)
    , dataReaderClose :: IO ()
    }

data DataWriter = DataWriter
    { dataWriterWrite :: ByteString -> IO ()
    , dataWriterClose :: IO ()
    }

data DataWriterDigest h = DataWriterDigest
    { dataWriterDigestWrite  :: ByteString -> IO ()
    , dataWriterDigestFinish :: Maybe (Digest h) -> IO ()
    }

filepathDataReader :: FilePath -> IO DataReader
filepathDataReader fp = do
    h <- openBinaryFile fp ReadMode
    let rC = do
            d <- B.hGet h 4096
            if B.null d
                then return Nothing
                else return $ Just d
    return $ DataReader { dataReaderRead  = rC
                        , dataReaderClose = hClose h
                        }

filepathDataWriter :: FilePath -> IO DataWriter
filepathDataWriter fp = do
    e <- doesFileExist fp
    if e
        then error "file already exist"
        else do
            h <- openBinaryFile fp WriteMode
            return $ DataWriter { dataWriterWrite = B.hPut h
                                , dataWriterClose = hClose h
                                }

pipeData :: DataReader -> DataWriter -> IO ()
pipeData dr dw = loop
  where
    loop = do
        mb <- dataReaderRead dr
        case mb of
            Nothing -> dataReaderClose dr >> dataWriterClose dw
            Just b  -> (dataWriterWrite dw) b >> loop

pipeDataDigest :: HashAlgorithm h
               => DataReader         -- ^ read data callback
               -> DataWriterDigest h -- ^ write data callback
               -> IO (Digest h)
pipeDataDigest dr dw =
    loop hashInitializeContext
  where
    loop !ctx = do
        mb <- dataReaderRead dr
        case mb of
            Nothing -> do
                let !digest = hashFinalize ctx
                dataReaderClose dr
                (dataWriterDigestFinish dw) (Just digest)
                return digest
            Just b  -> do
                (dataWriterDigestWrite dw) b
                loop (hashUpdate ctx b)
