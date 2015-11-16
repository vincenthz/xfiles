module Storage.HashFS
    ( initialize
    , HashFSConf(..)
    , makeConf
    , makeConfSHA256
    , makeConfSHA512
    , OutputDesc(..)
    , outputDigest
    , inputDigest
    , computeHash
    , onDigestFile
    , ImportType(..)
    , importFile
    , importFileAt
    , deleteFile
    , readFile
    , readInfo
    , verify
    , exists
    , iterateFiles
    , getPath
    ) where

import Data.Word (Word64)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Control.Applicative
import Control.Monad (when, unless, foldM, filterM, forM_)
import Control.Monad.Trans
import Control.Monad.Reader (ask)
import Storage.HashFS.Types
import Storage.HashFS.Local
import Storage.HashFS.Path
import Storage.HashFS.Utils
import Crypto.Hash
import System.IO hiding (readFile)
import Prelude hiding (readFile)
import Data.Time.Clock.POSIX (POSIXTime)

import System.Directory (createDirectoryIfMissing, removeFile, doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>), dropTrailingPathSeparator, dropFileName)
import System.Posix.Files hiding (isDirectory)

