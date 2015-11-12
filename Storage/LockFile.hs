module Storage.LockFile
    ( encryptChunkSize
    , decryptChunkSize
    , Header(..)
    , putHeader
    , getHeader
    , generateNonce
    , encryptChunk
    , decryptChunk
    ) where

import qualified Crypto.Cipher.ChaChaPoly1305 as ChaChaPoly1305
import qualified Crypto.MAC.Poly1305 as Poly1305
import           Crypto.Random
import           Crypto.Error
import           Data.Bits
import           Data.Word
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteArray.Pack as C
import qualified Data.ByteArray as B (convert)
import           Data.Memory.Endian

-- Header
data Header = Header
    { headerVersion   :: Int -- ^ 0 to 7 is valid
    , headerChunkMode :: Int -- ^ chunk mode. 0 = 4096 bytes, 1 = 8192 bytes, ..
    }

maximumChunkMode :: Int
maximumChunkMode = 4

maximumVersion :: Int
maximumVersion = 8

sizeFromChunkMode :: Int -> Int
sizeFromChunkMode chunkMode = 2 `shiftL` (12 + chunkMode)

encryptChunkSize :: Header -> Int
encryptChunkSize hdr = sizeFromChunkMode (headerChunkMode hdr)

decryptChunkSize :: Header -> Int
decryptChunkSize hdr = encryptChunkSize hdr + 12 + 16 -- adding poly1305 tag and 12 bytes nonce

maximumChunkSize :: Int
maximumChunkSize = sizeFromChunkMode maximumChunkMode + 12 + 16

putHeader :: Word32 -> Header -> ByteString
putHeader r (Header v csz) = either error id $ C.fill 4 $ C.putStorable (toLE m)
  where
    m :: Word32
    m = (r .&. headerMask) .|. (fromIntegral v `shiftL` 26) .|. (fromIntegral csz `shiftL` 13)

    headerMask :: Word32
    headerMask = 0xe3ff9fff

getHeader :: Word32 -> Header
getHeader w = Header (fromIntegral ver) (fromIntegral chunkMode)
  where
    ver       = (w `shiftR` 26) .&. 0x7
    chunkMode = (w `shiftR` 13) .&. 0x3

generateNonce :: IO ChaChaPoly1305.Nonce
generateNonce = 
    throwCryptoError . ChaChaPoly1305.nonce12 <$> (getRandomBytes 12 :: IO ByteString)

encryptChunk :: ChaChaPoly1305.Nonce -- ^ nonce: 12 bytes
             -> ByteString           -- ^ key: 16 bytes
             -> ByteString           -- ^ input: plaintext
             -> ByteString
encryptChunk nonce key inp =
    let st         = throwCryptoError $ ChaChaPoly1305.initialize key nonce
        st2        = ChaChaPoly1305.finalizeAAD st
        (out, st3) = ChaChaPoly1305.encrypt inp st2
        auth       = ChaChaPoly1305.finalize st3
     in B.concat [B.convert nonce,out,B.convert auth]

decryptChunk :: ByteString -- ^ key: 16 bytes
             -> ByteString -- ^ input: need to be at least 16+12 bytes
             -> Either String ByteString
decryptChunk key bs
    | len < 16+12            = Left "chunk too small"
    | len > maximumChunkSize = Left "chunk too big"
    | otherwise              =
         case Poly1305.authTag tagBs of
            CryptoFailed _           -> Left "auth tag not valid"
            CryptoPassed receivedTag ->
                let st         = throwCryptoError $ ChaChaPoly1305.initialize key n
                    st2        = ChaChaPoly1305.finalizeAAD st
                    (out, st3) = ChaChaPoly1305.decrypt ciphertext st2
                    auth       = ChaChaPoly1305.finalize st3
                 in if auth == receivedTag then Right out else Left "auth tag not matching"
  where
    len                  = B.length bs
    (nonce, cipherTagBs) = B.splitAt 12 bs
    (ciphertext, tagBs)  = B.splitAt ((len - 12) - 16) cipherTagBs
    n                    = throwCryptoError $ ChaChaPoly1305.nonce12 nonce
