module Main where

import           Network.Socket hiding (send,recv)
import           System.Timeout
import           System.Environment
import           Control.Monad
import           Control.Concurrent
import           Text.Read
import           Data.Maybe
import qualified Crypto.PubKey.Ed25519 as Ed25519
import           Crypto.Error
import qualified Tools.Config as Config

import qualified Data.ByteArray.Encoding as B
import qualified Data.ByteArray as B
import qualified Data.ByteString as B (readFile)
import qualified Data.ByteString.Char8 as BC

--import           Network.LSP
import           Storage.HashFS.Client
import           Storage.HashFS.Protocol
import           Storage.HashFS.IO
import           Storage.Utils

import           Hashfs.Common

main = do
    let serverAddr = "127.0.0.1"
        serverPort = 2902

    a <- getArgs

    key <- readKeyFile "client.priv"
    let servKey = parsePublicKeyHex (a !! 0)
    
    let conCfg = ConnectionConfig
            { concfgHostname    = serverAddr
            , concfgPort        = serverPort
            , concfgExpectedKey = servKey
            , concfgMyKey       = key
            }
    client <- clientNew conCfg
    clientEstablish client
    putStrLn ("connected to server on port " ++ show serverPort)

    -- FIXME do something

    clientShut client
    return ()
