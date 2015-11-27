module Storage.Utils
    ( ModificationTime
    , FileSize
    , getFileInfo
    ) where

import           System.Posix.Files hiding (isDirectory)
import           System.FilePath ((</>), dropTrailingPathSeparator, dropFileName)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Word

type ModificationTime = POSIXTime

type FileSize = Word64

-- | Return useful information about the file
getFileInfo :: FilePath -> IO (FileSize, ModificationTime)
getFileInfo path = do
    fstat <- getFileStatus path
    return ( fromIntegral $ fileSize fstat
           , realToFrac $ modificationTime fstat
           )
