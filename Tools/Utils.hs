module Tools.Utils
    ( Bytes
    , toBytes
    , BytesCondensed(..)
    , getFileSize
    ) where

import           Control.Applicative
import           Data.Monoid
import           Data.Word
import           System.IO
import           Text.Printf

newtype Bytes = Bytes Word64
    deriving (Eq,Ord)
newtype BytesCondensed = BytesCondensed Bytes
    deriving (Eq,Ord)

toBytes :: (Integral a, Num a) => a -> Bytes
toBytes n = Bytes (fromIntegral n)

instance Show Bytes where
    show (Bytes w) = showSZ w
instance Show BytesCondensed where
    show (BytesCondensed (Bytes w)) = showSZsmall w
instance Semigroup Bytes where
    (<>) (Bytes a) (Bytes b) = Bytes (a+b)
instance Monoid Bytes where
    mempty = Bytes 0
    mappend (Bytes a) (Bytes b) = Bytes (a+b)

showSZ :: Integral a => a -> String
showSZ sz
    | sz < 1024               = printf "%d" (fromIntegral sz :: Integer)
    | sz < 1024 * 1024        = printf "%.3fK" (fromIntegral sz / 1024 :: Double)
    | sz < 1024 * 1024 * 1024 = printf "%.3fM" (fromIntegral sz / (1024 * 1024) :: Double)
    | otherwise               = printf "%.4fG" (fromIntegral sz / (1024 * 1024 * 1024) :: Double)

showSZsmall :: Integral a => a -> String
showSZsmall sz
    | sz < 1024               = printf "%d" (fromIntegral sz :: Integer)
    | sz < 1024 * 1024        = printf "%.0fK" (fromIntegral sz / 1024 :: Double)
    | sz < 1024 * 1024 * 1024 = printf "%.1fM" (fromIntegral sz / (1024 * 1024) :: Double)
    | otherwise               = printf "%.2fG" (fromIntegral sz / (1024 * 1024 * 1024) :: Double)

getFileSize :: FilePath -> IO Bytes
getFileSize f = Bytes . fromIntegral <$> withFile f ReadMode hFileSize
