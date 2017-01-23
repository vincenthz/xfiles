module Storage.Utils
    ( ModificationTime
    , FileSize
    , getFileInfo
    , getFileInfoPlus
    ) where

import           System.Posix.Files hiding (isDirectory)
import           System.FilePath ((</>), dropTrailingPathSeparator, dropFileName)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Word
import           Data.Hourglass

type ModificationTime = Elapsed

type FileSize = Word64

-- | Return useful information about the file
getFileInfo :: FilePath -> IO (FileSize, ModificationTime)
getFileInfo path = do
    fstat <- getFileStatus path
    return ( fromIntegral $ fileSize fstat
           , Elapsed $ Seconds $ (floor $ (realToFrac $ modificationTime fstat :: Double))
           )

getFileInfoPlus :: FilePath -> IO (Maybe (FileSize, ModificationTime, Word64))
getFileInfoPlus path = do
    fstat <- getSymbolicLinkStatus path
    if isSymbolicLink fstat
        then return Nothing
        else do
            return $ Just
                ( fromIntegral $ fileSize fstat
                , Elapsed $ Seconds $ (floor $ (realToFrac $ modificationTime fstat :: Double))
                , fromIntegral $ linkCount fstat
                )
