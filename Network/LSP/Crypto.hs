module Network.LSP.Crypto
    ( NodeSecretKey
    , NodePublicKey
    , NodeSignature
    , SessionPublicKey
    , SessionSecretKey
    , SessionSharedKey
    , SymmetricKey
    , Curve25519.DhSecret
    , MasterSecret
    -- * Parsers
    , parseNodePublicKey
    , parseNodeSignature
    , parseSessionPublicKey
    -- * Generation / Computation
    , generateSessionKey
    , generateNewSession
    , generateMaster
    , generateCryptoState
    , computeSignature
    , verifySignature
    , prf
    -- * Encrypt & Decrypt
    , encrypt
    , decrypt
    ) where

import           Crypto.Error
import           Crypto.Random
import qualified Crypto.Cipher.ChaChaPoly1305 as ChaChaPoly1305
import qualified Crypto.MAC.Poly1305 as Poly1305
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Crypto.PubKey.Curve25519 as Curve25519

import           Crypto.MAC.HMAC
import           Crypto.Hash

import           Data.ByteString (ByteString)
import           Data.ByteArray (ScrubbedBytes, ByteArrayAccess, convert)
import qualified Data.ByteString as B (append, splitAt, length, concat)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteArray.Parse as P

import           Network.LSP.Types

type NodeSecretKey = Ed25519.SecretKey
type NodePublicKey = Ed25519.PublicKey

type NodeSignature = Ed25519.Signature

type SessionPublicKey = Curve25519.PublicKey
type SessionSecretKey = Curve25519.SecretKey
type SessionSharedKey = Curve25519.DhSecret

type SymmetricKey = ByteString

newtype MasterSecret = MasterSecret ByteString

handleParseCryptoError :: CryptoFailable a -> P.Parser ByteString a
handleParseCryptoError = either (fail . show) return . eitherCryptoError

parseNodePublicKey :: P.Parser ByteString NodePublicKey
parseNodePublicKey = P.take 32 >>= handleParseCryptoError . Ed25519.publicKey

parseNodeSignature :: P.Parser ByteString NodeSignature
parseNodeSignature = P.take 64 >>= handleParseCryptoError . Ed25519.signature

parseSessionPublicKey :: P.Parser ByteString SessionPublicKey
parseSessionPublicKey = P.take 32 >>= handleParseCryptoError . Curve25519.publicKey

generateSessionKey :: MonadRandom randomly
                   => randomly SessionSecretKey
generateSessionKey =
    throwCryptoError . Curve25519.secretKey . secureBytes <$> getRandomBytes 32
  where
    secureBytes :: ScrubbedBytes -> ScrubbedBytes
    secureBytes = id

generateNewSession :: MonadRandom randomly
                   => randomly (SessionRandom, SessionPublicKey -> SessionSharedKey, SessionPublicKey)
generateNewSession = do
    sessionKey <- generateSessionKey
    ran        <- generateSessionRandom
    return ( ran
           -- return it as a closure instead of the secret key to prevent an unintentional leakage.
           , flip Curve25519.dh sessionKey
           , Curve25519.toPublic sessionKey
           )

generateMaster :: SessionRandom -> SessionRandom -> Curve25519.DhSecret -> MasterSecret
generateMaster (SessionRandom r1) (SessionRandom r2) shared =
    let seed         = B.concat [BC.pack "master secret", r1, r2]
        masterSecret = prf seed (convert shared) 128
     in MasterSecret masterSecret

generateCryptoState :: MasterSecret -> (ByteString, ByteString, ByteString, ByteString)
generateCryptoState (MasterSecret ms1) =
    let (txKey, ms2) = B.splitAt 32 ms1
        (rxKey, ms3) = B.splitAt 32 ms2
        (iv1, ms4)   = B.splitAt 12 ms3
        (iv2, _)     = B.splitAt 12 ms4
     in (txKey, rxKey, iv1, iv2)

computeSignature :: ByteArrayAccess ba => (NodeSecretKey, NodePublicKey) -> ba -> NodeSignature
computeSignature (nodeKey, nodePubKey) dat =
    Ed25519.sign nodeKey nodePubKey dat

verifySignature :: NodePublicKey -> NodeSignature -> ByteString -> Bool
verifySignature nodePubKey sig dat =
    Ed25519.verify nodePubKey dat sig

encrypt :: SymmetricKey -> ByteString -> ByteString -> ByteString -> ByteString
encrypt key nonce hdr inp =
    let st         = throwCryptoError $ ChaChaPoly1305.initialize key n
        st2        = ChaChaPoly1305.finalizeAAD $ ChaChaPoly1305.appendAAD hdr st
        (out, st3) = ChaChaPoly1305.encrypt inp st2
        auth       = ChaChaPoly1305.finalize st3
     in out `B.append` convert auth
  where
    n = throwCryptoError $ ChaChaPoly1305.nonce12 nonce

decrypt :: SymmetricKey -> ByteString -> ByteString -> ByteString -> Maybe ByteString
decrypt key nonce hdr inp
    | len < 16  = Nothing
    | otherwise =
         case Poly1305.authTag tagBS of
            CryptoFailed _ -> Nothing
            CryptoPassed receivedTag ->
                let st         = throwCryptoError $ ChaChaPoly1305.initialize key n
                    st2        = ChaChaPoly1305.finalizeAAD $ ChaChaPoly1305.appendAAD hdr st
                    (out, st3) = ChaChaPoly1305.decrypt ciphertext st2
                    auth       = ChaChaPoly1305.finalize st3
                 in if auth == receivedTag then Just out else Nothing
  where
    len                 = B.length inp
    (ciphertext, tagBS) = B.splitAt (len - 16) inp
    n                   = throwCryptoError $ ChaChaPoly1305.nonce12 nonce

-- | Same PRF used in TLS
--
-- P_hash(secret, seed) = HMAC_hash(secret, A(1) + seed) +
--                        HMAC_hash(secret, A(2) + seed) +
--                        HMAC_hash(secret, A(3) + seed) + ...
--
-- A(0) = seed
-- A(i) = HMAC(secret, A(i-1))
--
prf :: ByteString -> ByteString -> Int -> ByteString
prf seed secret nbBytes =
    B.concat $ map convert $ loop nbBytes a0
  where
    dLen = 64

    hmacSHA512 :: ByteString -> Digest SHA512
    hmacSHA512 msg = hmacGetDigest $ hmac secret msg

    a0 = hmacSHA512 seed

    loop :: Int -> Digest SHA512 -> [Digest SHA512]
    loop len aPrev
        | dLen >= len = [out]
        | otherwise   = out : loop (len - dLen) aN
      where
        aN  = hmacSHA512 (convert aPrev)
        out = hmacSHA512 (B.append (convert aN) seed)
