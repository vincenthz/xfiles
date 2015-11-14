module Storage.LockFile
    ( encryptChunkSize
    , decryptChunkSize
    , Header(..)
    -- * Semi internal part
    , putHeader
    , getHeader
    , generateNonce
    , encryptChunk
    , decryptChunk
    -- * Lazy and simple interface
    , encrypt
    , decrypt
    ) where

import qualified Crypto.Cipher.ChaChaPoly1305 as ChaChaPoly1305
import qualified Crypto.MAC.Poly1305 as Poly1305
import           Crypto.Random
import           Control.Exception (throw, Exception)
import           Crypto.Error
import           Data.Bits
import           Data.Word
import           Data.Typeable
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteArray.Pack as C
import qualified Data.ByteArray.Parse as P
import qualified Data.ByteArray as B (convert)
import           Data.Memory.Endian

-- Header
data Header = Header
    { headerVersion   :: Int -- ^ 0 to 7 is valid
    , headerChunkMode :: Int -- ^ chunk mode. 0 = 4096 bytes, 1 = 8192 bytes, ..
    }

type Key = ByteString

data ChunkError =
      ChunkTooSmall
    | ChunkTooBig
    | ChunkAuthTagInvalid
    | ChunkAuthTagFailure
    deriving (Show,Eq,Typeable)

instance Exception ChunkError

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

getHeader :: ByteString -> Maybe Header
getHeader bs =
    case P.parse (P.takeStorable) bs of
        P.ParseFail _ -> Nothing
        P.ParseMore _ -> Nothing
        P.ParseOK _ w -> Just $ getW (fromLE w)
  where
    getW :: Word32 -> Header
    getW w = Header (fromIntegral ver) (fromIntegral chunkMode)
      where
        ver       = (w `shiftR` 26) .&. 0x7
        chunkMode = (w `shiftR` 13) .&. 0x3

generateNonce :: MonadRandom randomly => randomly ChaChaPoly1305.Nonce
generateNonce =
    throwCryptoError . ChaChaPoly1305.nonce12 . witness <$> getRandomBytes 12 -- (getRandomBytes 12 :: randomly ByteString)
  where
    witness :: ByteString -> ByteString
    witness = id

encryptChunk :: ChaChaPoly1305.Nonce -- ^ nonce: 12 bytes
             -> Key                  -- ^ key: 16 bytes
             -> ByteString           -- ^ input: plaintext
             -> ByteString
encryptChunk nonce key inp =
    let st         = throwCryptoError $ ChaChaPoly1305.initialize key nonce
        st2        = ChaChaPoly1305.finalizeAAD st
        (out, st3) = ChaChaPoly1305.encrypt inp st2
        auth       = ChaChaPoly1305.finalize st3
     in B.concat [B.convert nonce,out,B.convert auth]

decryptChunk :: Key        -- ^ key: 16 bytes
             -> ByteString -- ^ input: need to be at least 16+12 bytes
             -> Either ChunkError ByteString
decryptChunk key bs
    | len < 16+12            = Left ChunkTooSmall
    | len > maximumChunkSize = Left ChunkTooBig
    | otherwise              =
         case Poly1305.authTag tagBs of
            CryptoFailed _           -> Left ChunkAuthTagInvalid
            CryptoPassed receivedTag ->
                let st         = throwCryptoError $ ChaChaPoly1305.initialize key n
                    st2        = ChaChaPoly1305.finalizeAAD st
                    (out, st3) = ChaChaPoly1305.decrypt ciphertext st2
                    auth       = ChaChaPoly1305.finalize st3
                 in if auth == receivedTag then Right out else Left ChunkAuthTagFailure
  where
    len                  = B.length bs
    (nonce, cipherTagBs) = B.splitAt 12 bs
    (ciphertext, tagBs)  = B.splitAt ((len - 12) - 16) cipherTagBs
    n                    = throwCryptoError $ ChaChaPoly1305.nonce12 nonce

-- | Lazily encrypt a datastream to a LockFile
encrypt :: DRG gen
        => gen
        -> Header       -- ^ Header
        -> Key          -- ^ Key
        -> L.ByteString -- ^ input
        -> L.ByteString
encrypt drgOrig hdr key dataStream = L.fromStrict (putHeader 0x12345678 hdr) `L.append` loop drgOrig dataStream
  where
    loop drg lbs
        | L.null lbs = L.empty
        | otherwise  =
            let (l1,l2)       = L.splitAt (fromIntegral $ encryptChunkSize hdr) lbs
                chunkData     = B.concat $ L.toChunks l1
                (nonce, drg') = withDRG drg generateNonce
             in L.fromStrict (encryptChunk nonce key chunkData) `L.append` loop drg' l2

-- | Lazyily decrypt a LockFile datastream
decrypt :: Key
        -> L.ByteString
        -> Maybe L.ByteString
decrypt key lockFileStream =
    let (l1, l2) = L.splitAt 4 lockFileStream
     in decryptWithHeader l2 <$> getHeader (L.toStrict l1)
  where
    decryptWithHeader :: L.ByteString -> Header -> L.ByteString
    decryptWithHeader dataStream hdr = loop dataStream
      where
        loop lbs
            | L.null lbs = L.empty
            | otherwise  =
                let (l1, l2)  = L.splitAt (fromIntegral $ decryptChunkSize hdr) lbs
                 in case decryptChunk key (L.toStrict l1) of
                        Left err  -> throw err
                        Right out -> L.fromStrict out `L.append` loop l2
