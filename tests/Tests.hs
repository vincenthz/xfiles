{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Monadic

import           Data.Maybe

import qualified Data.ByteString as B
import qualified Data.ByteArray as B (convert)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import           Network.LSP
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

instance Show Ed25519.SecretKey where
    show key = show (B.convert key :: B.ByteString)

arbitraryEd25519Key :: Gen Ed25519.SecretKey
arbitraryEd25519Key = throwCryptoError . Ed25519.secretKey . B.pack <$> replicateM 32 arbitrary

arbitraryChunks :: (Int, Int) -> (Int, Int) -> Gen [B.ByteString]
arbitraryChunks nbChunks szChunks = do
    nb <- choose nbChunks
    let arbitraryBs = do
            len <- choose szChunks
            B.pack <$> replicateM len arbitrary
    replicateM nb arbitraryBs

runClientServer runClient runServer = run $ do
    clientBackend <- newFakeHandle
    serverBackend <- newFakeHandle

    pipeFakeHandle clientBackend serverBackend
    concurrently (runClient clientBackend) (runServer serverBackend)

prepareWorkingConfig :: Ed25519.SecretKey -> Ed25519.SecretKey -> (Config, Config)
prepareWorkingConfig clientPriv serverPriv =
    ( Config (clientPriv, clientPub) [serverPub]
    , Config (serverPriv, serverPub) [clientPub]
    )
  where
    clientPub = Ed25519.toPublic clientPriv
    serverPub = Ed25519.toPublic serverPriv

propHandshake :: PropertyM IO ()
propHandshake = do
    (clientPriv, serverPriv) <- pick ((,) <$> arbitraryEd25519Key
                                          <*> arbitraryEd25519Key)
    let (clientCfg, serverCfg) = prepareWorkingConfig clientPriv serverPriv

    let runClient backend = do
            _ <- client backend clientCfg
            return ()
        runServer backend = do
            _ <- server backend serverCfg
            return ()
    ((),()) <- runClientServer runClient runServer
    return ()

propSendData :: PropertyM IO ()
propSendData = do
    (inp, clientPriv, serverPriv) <- pick ((,,) <$> arbitraryChunks (1,6) (1,2000)
                                                <*> arbitraryEd25519Key
                                                <*> arbitraryEd25519Key)
    let (clientCfg, serverCfg) = prepareWorkingConfig clientPriv serverPriv

    let runClient backend = do
            lsp <- client backend clientCfg
            mapM_ (send lsp) inp
            return True
        runServer backend = do
            lsp <- server backend serverCfg
            r' <- replicateM (length inp) $ recv lsp
            return r'
    (_,r') <- runClientServer runClient runServer
    assertEq inp r'

propRecvData :: PropertyM IO ()
propRecvData = do
    (inp, clientPriv, serverPriv) <- pick ((,,) <$> arbitraryChunks (1,6) (1,2000)
                                                <*> arbitraryEd25519Key
                                                <*> arbitraryEd25519Key)

    let (clientCfg, serverCfg) = prepareWorkingConfig clientPriv serverPriv

    let runClient backend = do
            lsp <- client backend clientCfg
            r' <- replicateM (length inp) $ recv lsp
            return r'
        runServer backend = do
            lsp <- server backend serverCfg
            mapM_ (send lsp) inp
            return True

    (r',_) <- runClientServer runClient runServer
    assertEq inp r'

assertEq :: (Show a, Monad m, Eq a) => a -> a -> m ()
assertEq expected got = unless (expected == got) $ error ("got " ++ show got ++ " but was expecting " ++ show expected)

main :: IO ()
main = defaultMain $ testGroup "lsp"
    [ testGroup "handshake"
        [ testProperty "handshake" (monadicIO propHandshake)
        ]
    , testGroup "data"
        [ testProperty "send data" (monadicIO propSendData)
        , testProperty "recv data" (monadicIO propRecvData)
        ]
    ]
