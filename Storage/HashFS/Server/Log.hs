module Storage.HashFS.Server.Log
    ( LogEvent(..)
    , catchAllPrint
    ) where

import Network.Socket
import Control.Exception

data LogEvent =
      ClientConnected SockAddr
    | ClientEstablished SockAddr
    | ClientLeft SockAddr
    | ServerListening
    deriving (Show,Eq)


catchAllPrint :: String -> IO () -> IO ()
catchAllPrint context f = catch f printExn
  where
    printExn :: SomeException -> IO ()
    printExn e = putStrLn (context ++ " exception: " ++ show e)