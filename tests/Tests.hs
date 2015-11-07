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

import           Control.Concurrent.Async

import           FakeHandle

propSendData :: PropertyM IO ()
propSendData = do
    clientPriv <- run (throwCryptoError . Ed25519.secretKey . witness <$> getRandomBytes 32)
    serverPriv <- run (throwCryptoError . Ed25519.secretKey . witness <$> getRandomBytes 32)

    let clientPub = Ed25519.toPublic clientPriv
        serverPub = Ed25519.toPublic serverPriv

    let clientCfg = Config (clientPriv, clientPub) [serverPub]
        serverCfg = Config (serverPriv, serverPub) [clientPub]

    inp <- pick $ do
                nbChunks <- choose (1,6)
                let arbitraryBs = do
                        len <- choose (1,2000)
                        B.pack <$> replicateM len arbitrary
                replicateM nbChunks arbitraryBs

    run $ do
        clientBackend <- newFakeHandle
        serverBackend <- newFakeHandle

        pipeFakeHandle clientBackend serverBackend

        let runClient = do
                clientLSP <- client clientBackend clientCfg
                mapM_ (send clientLSP) inp
                return True
            runServer = do
                serverLSP <- server serverBackend serverCfg
                r' <- replicateM (length inp) $ recv serverLSP
                return r'

        (_, r') <- concurrently runClient runServer

        assertEq inp r'
  where
    witness :: B.ByteString -> B.ByteString
    witness = id

propRecvData :: PropertyM IO ()
propRecvData = do
    clientPriv <- run (throwCryptoError . Ed25519.secretKey . witness <$> getRandomBytes 32)
    serverPriv <- run (throwCryptoError . Ed25519.secretKey . witness <$> getRandomBytes 32)

    let clientPub = Ed25519.toPublic clientPriv
        serverPub = Ed25519.toPublic serverPriv

    let clientCfg = Config (clientPriv, clientPub) [serverPub]
        serverCfg = Config (serverPriv, serverPub) [clientPub]

    inp <- pick $ do
                nbChunks <- choose (1,6)
                let arbitraryBs = do
                        len <- choose (1,2000)
                        B.pack <$> replicateM len arbitrary
                replicateM nbChunks arbitraryBs

    run $ do
        clientBackend <- newFakeHandle
        serverBackend <- newFakeHandle

        pipeFakeHandle clientBackend serverBackend

        let runClient = do
                lsp <- client clientBackend clientCfg
                r' <- replicateM (length inp) $ recv lsp
                return r'
            runServer = do
                lsp <- server serverBackend serverCfg
                mapM_ (send lsp) inp
                return True

        (_, r') <- concurrently runServer runClient

        assertEq inp r'
  where
    witness :: B.ByteString -> B.ByteString
    witness = id

assertEq :: (Show a, Monad m, Eq a) => a -> a -> m ()
assertEq expected got = unless (expected == got) $ error ("got " ++ show got ++ " but was expecting " ++ show expected)

main :: IO ()
main = defaultMain $ testGroup "lsp"
    [ tests_handshake
    ]
  where
        -- high level tests between a client and server with fake ciphers.
        tests_handshake = testGroup "Handshakes"
            [ testProperty "send data" (monadicIO propSendData)
            , testProperty "recv data" (monadicIO propRecvData)
            ]
