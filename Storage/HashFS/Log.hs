module Storage.HashFS.Log
    ( LogHandle
    , logOpen
    , logClose
    , logFlush
    , logAppend
    ) where

import System.Hourglass
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Sql

data LogHandle = LogHandle

logOpen :: FilePath -> IO LogHandle
logOpen dir = undefined

logAppend :: LogHandle -> String -> IO ()
logAppend logHandle s = undefined

logFlush :: LogHandle -> IO ()
logFlush logHandle = undefined

logClose :: LogHandle -> IO ()
logClose logHandle = undefined
