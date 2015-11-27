module Storage.HashFS.Server
    ( Server
    , serverNew
    , serverShut
    ) where

import           Control.Concurrent.MVar

import qualified Data.ByteArray.Pack as C
import qualified Data.ByteArray.Parse as P
import           Data.ByteString (ByteString)
import           Data.Memory.Endian
import           Data.Word

import           Network.Socket hiding (send, recv)
import           Network.LSP
import qualified Data.ByteString as B
import qualified Control.Exception as E
import           Data.Typeable

import           Storage.HashFS.Types
import           Storage.HashFS.Hasher
import           Storage.HashFS.IO
import           Storage.HashFS.Protocol
import           Storage.HashFS.ProtocolUtils

data Server = Server Socket LSP

serverNew :: IO Server
serverNew = return $ Server undefined undefined

serverShut :: Server -> IO ()
serverShut (Server sock _) = do
    -- FIXME shutdown lsp too ?
    shutdown sock ShutdownBoth
