-- Basic types and associated functions
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.LSP.Types
    ( Version(..)
    , SessionRandom(..)
    , generateSessionRandom
    ) where

import           Crypto.Random
import           Data.Word
import           Data.ByteString (ByteString)
import           Data.ByteArray (ByteArrayAccess)

newtype Version = Version Word16
    deriving (Show,Eq)

newtype SessionRandom = SessionRandom ByteString
    deriving (Show,Eq,ByteArrayAccess)

generateSessionRandom :: MonadRandom randomly => randomly SessionRandom
generateSessionRandom = SessionRandom <$> getRandomBytes 32
