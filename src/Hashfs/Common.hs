module Hashfs.Common
    ( readKeyFile
    , parsePublicKeyHex
    ) where

import           Network.LSP (NodeSecretKey, NodePublicKey)
import           Crypto.Error
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteArray.Encoding as B
import qualified Data.ByteArray as B
import qualified Data.ByteString as B (readFile)
import qualified Data.ByteString.Char8 as BC

readKeyFile :: FilePath -> IO (NodeSecretKey, NodePublicKey)
readKeyFile privateKeyFilePath = do
    keyFile <- B.readFile privateKeyFilePath
    case Ed25519.secretKey keyFile of
        CryptoFailed err -> error ("key file " ++ privateKeyFilePath ++ " in listen section invalid: " ++ show err)
        CryptoPassed r   -> return (r, Ed25519.toPublic r)

parsePublicKeyHex :: String -> NodePublicKey
parsePublicKeyHex p =
    case B.convertFromBase B.Base16 $ BC.pack p of
        Left err -> error ("pubkey base16 is invalid : " ++ show err)
        Right bs -> case Ed25519.publicKey (bs :: BC.ByteString) of
            CryptoFailed err -> error ("pubkey invalid: " ++ show err)
            CryptoPassed r   -> r
