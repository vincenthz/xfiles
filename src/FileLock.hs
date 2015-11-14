{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- an old encryption program. do not use anymore.
module Main where

import qualified Crypto.KDF.PBKDF2 as PBKDF2
import Crypto.Hash
import Crypto.Error
import Crypto.Random
import Control.Applicative
import Control.Exception
import System.IO
import System.Environment
import Storage.LockFile
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteArray as BA

askPasswd :: IO String
askPasswd = bracket
    (hSetEcho stdin False)
    (\() -> hSetEcho stdin True)
    (\() -> hGetLine stdin)

getKey :: IO ByteString
getKey = do
    putStrLn "enter passphrase:"
    passwd <- UTF8.fromString <$> askPasswd
    putStrLn $ show $ B.length $ stretched passwd
    return $ stretched passwd
  where
    stretched :: B.ByteString -> B.ByteString
    stretched passwd = PBKDF2.generate (PBKDF2.prfHMAC SHA512) (PBKDF2.Parameters 4096 32) passwd salt

salt :: B.ByteString
salt = "ctrfile"

readAsMuch :: Int -> Handle -> IO ByteString
readAsMuch bytes handle = B.concat <$> loop bytes
  where
    loop n
        | n == 0    = return []
        | otherwise = do
            chunk <- B.hGet handle (min 4096 n)
            if B.null chunk
                then return []
                else ((:) chunk) <$> loop (n - B.length chunk)

doDecrypt file key =
    withFile file ReadMode $ \h ->
    withFile (file ++ ".decrypt") WriteMode $ \hout -> do
        hdrBs <- B.hGet h 4
        let hdr = Header 1 0
            loop = do
                inp <- readAsMuch (decryptChunkSize hdr) h
                if B.null inp
                    then return ()
                    else do
                        case decryptChunk key inp of
                            Left err  -> error ("chunk failure: " ++ show err)
                            Right out -> B.hPut hout out
                        loop
        loop

doEncrypt hdr file key =
    withFile file ReadMode $ \h ->
    withFile (file ++ ".locked") WriteMode $ \hout -> do
        let hdrBs = putHeader 0x12345678 hdr
            loop = do
                inp <- readAsMuch (encryptChunkSize hdr) h
                if B.null inp
                    then return ()
                    else do
                        nonce <- generateNonce
                        let out = encryptChunk nonce key inp
                        B.hPut hout out
                        loop
        B.hPut hout hdrBs
        loop

main = do
    args <- getArgs
    let hdr = Header 1 0
    case args of
        "decrypt":file:[] -> getKey >>= doDecrypt file
        "encrypt":file:[] -> getKey >>= doEncrypt hdr file
        _                 -> error "unknown command"
