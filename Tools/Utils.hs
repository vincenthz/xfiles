module Tools.Utils
    ( Bytes
    , getFileSize
    ) where

import           Tools.ChronoFs.Utils (showSZ)
import           Control.Applicative
import           Data.Monoid
import           Data.Word
import           System.IO

newtype Bytes = Bytes Word64
    deriving (Eq)

instance Show Bytes where
    show (Bytes w) = showSZ w
instance Monoid Bytes where
    mempty = Bytes 0
    mappend (Bytes a) (Bytes b) = Bytes (a+b)

getFileSize :: FilePath -> IO Bytes
getFileSize f = Bytes . fromIntegral <$> withFile f ReadMode hFileSize
