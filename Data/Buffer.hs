module Data.Buffer where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as B
import           Data.Monoid
import           System.IO

data Buffer = Buffer
    { bufferBuf :: [ByteString]
    }

instance Monoid Buffer where
    mempty = Buffer []
    mappend b1 b2 = mconcat [b1,b2]
    mconcat = Buffer . mconcat . map (reverse . bufferBuf)

bufferInit :: Buffer
bufferInit = Buffer []

bufferAppend :: ByteString -> Buffer -> Buffer
bufferAppend u (Buffer l) = Buffer (u:l)

bufferAppendString :: String -> Buffer -> Buffer
bufferAppendString s = bufferAppend (UTF8.fromString s)

bufferFlushTo :: Handle -> Buffer -> IO ()
bufferFlushTo handle buf =
    mapM_ (B.hPut handle) (reverse $ bufferBuf buf)
