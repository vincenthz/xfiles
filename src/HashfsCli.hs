module Main where

import           Network.Socket hiding (send,recv)
import           System.Timeout
import           System.Environment
import           Control.Monad
import           Control.Concurrent
import           Text.Read
import           Data.Maybe
import           Data.Monoid
import qualified Crypto.PubKey.Ed25519 as Ed25519
import           Crypto.Error
--import qualified Tools.Config as Config

import qualified Data.ByteArray.Encoding as B
import qualified Data.ByteArray as B
import qualified Data.ByteString as B (readFile)
import qualified Data.ByteString.Char8 as BC

--import           Network.LSP
import           Storage.HashFS
import           Storage.HashFS.Client
import           Storage.HashFS.Protocol
import           Storage.HashFS.IO
import           Storage.Utils

import           Hashfs.Common

import           Console.Options

serverAddr :: String
serverAddr = "127.0.0.1"

serverPort :: Int
serverPort = 2902

main :: IO ()
main = defaultMain $ do
    programName "hashfs-cli"
    programDescription "hashfs -- command line interface"

    cfgFlag <- flag $ FlagLong "config"
                   <> FlagShort 'c'
                   <> FlagDescription "client configuration to use (default \"client.priv\")"
                   -- <> FlagDefault "client.priv"
                   -- <> FlagArg ""

    command "replicate" $ do
        serverFlag <- flag $ FlagLong "server"
        serverKey  <- flag $ FlagLong "server-key"
        action $ \_ _ -> do
            key <- readKeyFile "client.priv"
            --a <- getArgs
            -- hardcoded server public key in hex
            let a = ["fb05f8e67af98044cee008712ff175da695b2b18ed65c1ca47f5f07e73ddfed9"]
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

    command "config" $ do
        action $ \_ _ -> do
            withConfig $ \providers -> do
                mapM_ (putStrLn . show) providers
