{-# LANGUAGE OverloadedStrings #-}
module Storage.HashFS.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified Data.ByteArray.Encoding as B

import Storage.HashFS.Hasher

-- | Configuration for HashFS
data HashFSConf h = HashFSConf
    {
    -- | specify how to split the hash into directory and file.
    -- e.g. [1,2] will split into a path a/bc/res
      hashfsDepth :: [Int]
    -- | the hash function used
    , hashfsHash  :: Hasher h
    -- | The ascii function used
    , hashfsOutputDesc :: OutputDesc
    -- | Root of the hash filesystem
    , hashfsRoot :: FilePath
    } deriving (Show,Eq)

-- | Internal variant to handle output format of ASCII
data OutputFormat = OutputString String | OutputByteString ByteString

lengthOutputFormat :: OutputFormat -> Int
lengthOutputFormat (OutputString s) = length s
lengthOutputFormat (OutputByteString s) = B.length s

-- | type of encoding for digest output
data OutputDesc =
      OutputHex    -- ^ hexadecimal, base16 output
    | OutputBase32 -- ^ base32 with / as _. more efficient than hex and work on CI filesystem
    | OutputBase64 -- ^ base64 with / as _. doesn't work on CI filesystem.
    -- | OutputCustom (Char -> Bool) (Digest h -> OutputFormat) (OutputFormat -> Digest h)
    deriving (Show,Read,Eq)

validChar :: OutputDesc -> Char -> Bool
validChar OutputHex c
    | fromEnum c < 0x80 = maybe False (const True) $ B.elemIndex (fromIntegral $ fromEnum c) base16Dict
    | otherwise         = False
validChar OutputBase32 c
    | fromEnum c < 0x80 = maybe False (const True) $ B.elemIndex (fromIntegral $ fromEnum c) base32Dict
    | otherwise         = False
validChar OutputBase64 c
    | fromEnum c < 0x80 = maybe False (const True) $ B.elemIndex (fromIntegral $ fromEnum c) base64Dict
    | otherwise         = False

-- | Transform a digest into a String
outputDigest :: OutputDesc -> Digest h -> String
outputDigest OutputHex digest    = show digest
outputDigest OutputBase32 digest = removePadding $ BC.unpack (B.convertToBase B.Base32 digest :: ByteString)
  where removePadding [] = []
        removePadding ('=':xs) = removePadding xs
        removePadding (x:xs)   = x:removePadding xs
outputDigest OutputBase64 digest = recode $ BC.unpack (B.convertToBase B.Base64 digest :: ByteString)
  where
    -- remove the padding and replace '/' by '_'
    recode []           = []
    recode ('=':[])     = []
    recode ('=':'=':[]) = []
    recode (x:xs) | x == '/'  = '_':recode xs
                  | otherwise = x:recode xs

-- | input a string and try to transform to a digest
inputDigest :: HashAlgorithm h => OutputDesc -> String -> Maybe (Digest h)
inputDigest OutputHex s
    | len == 0 || odd len                        = Nothing
    | and (map (validChar OutputHex) s) == False = Nothing
    | otherwise                                  = digestFromByteString $ B.pack $ loop s
  where len = length s
        loop []      = []
        loop (_:[])  = error "impossible"
        loop (x:y:z) = unhex x * 16 + unhex y : loop z
        unhex c | c >= '0' && c <= '9' = fromIntegral (fromEnum c - fromEnum '0')
                | c >= 'a' && c <= 'f' = fromIntegral (fromEnum c - fromEnum 'a' + 10)
                | otherwise            = error "invalid character"
inputDigest OutputBase32 s
    | and (map (validChar OutputBase32) s) == False = Nothing
    | otherwise                                     =
        case B.convertFromBase B.Base32 (BC.pack s) of
            Left _   -> Nothing
            Right b  -> digestFromByteString (b :: ByteString)
inputDigest OutputBase64 s
    | len == 0  = Nothing
    | otherwise = Nothing
  where len = length s

base16Dict :: ByteString
base16Dict = "0123456789abcdef"

-- rfc 4648
base32Dict :: ByteString
base32Dict = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"

base64Dict :: ByteString
base64Dict = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+_"
