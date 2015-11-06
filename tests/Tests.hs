{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Monadic

import           Data.Maybe

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import           Network.LSP
import           Network.Socket hiding (send, recv)
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad

import           Data.IORef

import           System.Timeout
import           System.Environment

import qualified Crypto.PubKey.Ed25519 as Ed25519
import           Crypto.Error
import           Crypto.Random

{-
prop_pipe_work :: PropertyM IO ()
prop_pipe_work = do
    clientPriv <- run (throwCryptoError . Ed25519.secretKey <$> getRandomBytes 32)
    serverPriv <- run (throwCryptoError . Ed25519.secretKey <$> getRandomBytes 32)

    let clientPub = Ed25519.toPublic clientPriv
        serverPub = Ed25519.toPublic serverPriv

    let clientCfg = Config (clientPriv, clientPub) [serverPub]
        serverCfg = Config (serverPriv, serverPub) [clientPub]

    run $ do
        let clientBackend = undefined
            serverBackend = undefined

        clientLSP <- client clientBackend clientCfg
        serverLSP <- server serverBackend serverCfg

        let r = "hello"
        send clientLSP r
        r' <- recv serverLSP
    
        return $ r == r'

    return ()

main :: IO ()
main = defaultMain $ testGroup "lsp"
    [ tests_handshake
    ]
  where
        -- high level tests between a client and server with fake ciphers.
        tests_handshake = testGroup "Handshakes"
            [ testProperty "setup" (monadicIO prop_pipe_work)
            ]
-}

main = do
    let clientPriv = throwCryptoError . Ed25519.secretKey $ B.replicate 32 3
        serverPriv = throwCryptoError . Ed25519.secretKey $ B.replicate 32 6
        clientPub = Ed25519.toPublic clientPriv
        serverPub = Ed25519.toPublic serverPriv
        clientCfg = Config (clientPriv, clientPub) [serverPub]
        serverCfg = Config (serverPriv, serverPub) [clientPub]
    args <- getArgs
    addr <- inet_addr "127.0.0.1"
    case args of
        "client":_ -> do
            sock <- socket AF_INET Stream 0
            let sockaddr = SockAddrInet 3999 addr
            connect sock sockaddr

            clientLSP <- client sock clientCfg

            send clientLSP "HELLO"

        "server":_ -> do
            sock <- socket AF_INET Stream 0
            let sockaddr = SockAddrInet 3999 addr
            bind sock sockaddr
            listen sock 10
            (x,_) <- accept sock
            serverLSP <- server x serverCfg
            
            x <- recv serverLSP
            putStrLn $ "received " ++ show x
        []         -> error "usage: $0 <client|server>"
