module Data.Buffer where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as B
import           Data.Monoid
import           System.IO
import           Basement.Compat.Semigroup

data Buffer = Buffer
    { bufferBuf :: [ByteString]
    }

instance Semigroup Buffer where
    (<>) b1 b2 = bufferConcat [b1,b2]
instance Monoid Buffer where
    mempty = Buffer []
    mappend b1 b2 = mconcat [b1,b2]
    mconcat = bufferConcat

bufferInit :: Buffer
bufferInit = Buffer []

bufferAppend :: ByteString -> Buffer -> Buffer
bufferAppend u (Buffer l) = Buffer (u:l)

bufferConcat :: [Buffer] -> Buffer
bufferConcat = Buffer . mconcat . map (reverse . bufferBuf)

bufferAppendString :: String -> Buffer -> Buffer
bufferAppendString s = bufferAppend (UTF8.fromString s)

bufferFlushTo :: Handle -> Buffer -> IO ()
bufferFlushTo handle buf =
    mapM_ (B.hPut handle) (reverse $ bufferBuf buf)
