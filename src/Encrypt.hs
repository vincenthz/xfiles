{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- an old encryption program. do not use anymore.
module Main where

import qualified Crypto.KDF.PBKDF2 as PBKDF2
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Hash
import Crypto.Error
import Control.Applicative
import Control.Exception
import System.IO
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteArray as BA

askPasswd :: IO String
askPasswd = bracket
    (hSetEcho stdin False)
    (\() -> hSetEcho stdin True)
    (\() -> hGetLine stdin)

getKey :: IO AES256
getKey = do
    passwd <- UTF8.fromString <$> askPasswd
    return $ throwCryptoError $ cipherInit $ stretched passwd
  where
    stretched :: B.ByteString -> B.ByteString
    stretched passwd = PBKDF2.generate (PBKDF2.prfHMAC SHA512) (PBKDF2.Parameters 4096 32) passwd salt

salt :: B.ByteString
salt = "ctrfile"

doCtr key startIv hIn hOut = loop startIv
  where loop iv = do
            content <- B.hGet hIn 1024
            B.hPut hOut $ ctrCombine key iv content
            if B.length content < 1024
                then return ()
                else loop (ivAdd iv (1024 `div` 16))

doDecrypt file key =
    withFile file ReadMode $ \h ->
    withFile (file ++ ".decrypt") WriteMode $ \hout -> do
        iv <- maybe (error "cannot get IV") id . makeIV <$> B.hGet h 16
        doCtr key iv h hout


doEncrypt file key = do
    iv <- withFile "/dev/urandom" ReadMode $ \h -> (maybe (error "cannot get IV") id . makeIV <$> B.hGet h 16)
    withFile file ReadMode $ \h ->
        withFile (file ++ ".encrypt") WriteMode $ \hout -> do
            B.hPut hout $ BA.convert iv
            doCtr key iv h hout

main = do
    args <- getArgs
    case args of
        "decrypt":file:[] -> getKey >>= doDecrypt file
        "encrypt":file:[] -> getKey >>= doEncrypt file
        _                 -> error "unknown command"
