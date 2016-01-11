{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tools.Quarry.Monad
    where

import Control.Applicative
import Control.Monad.Reader
import Tools.Quarry.Config
import Crypto.Hash (HashAlgorithm)

-- | Quarry Monad
newtype QuarryM h a = QuarryM { runQuarryM :: ReaderT (QuarryConfig h) IO a }
    deriving (Functor, Applicative, Monad, MonadReader (QuarryConfig h), MonadIO)

runQuarry :: (Show h, HashAlgorithm h) => QuarryConfig h -> QuarryM h a -> IO a
runQuarry conf f =
    runReaderT (runQuarryM f) conf
