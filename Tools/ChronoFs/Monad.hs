{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tools.ChronoFs.Monad
    ( BackupConfig(..)
    , defaultBackupConfig
    , BackupState(..)
    , BackupRO
    , runBackupRO
    , Backup
    , runBackup
    , incErrors
    , incDups
    , incSkipped
    , incProcessed
    , addBytes
    ) where

import Filesystem.Path.CurrentOS
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Data.Word
import Tools.ChronoFs.Types
import System.Console.Terminfo (Terminal)
import Data.FileFormat
import Prelude hiding (FilePath)

data BackupConfig = BackupConfig
    { backupDir          :: FilePath
    , backupUseHardlinks :: Bool
    } deriving (Show,Eq)

defaultBackupConfig :: FilePath -> BackupConfig
defaultBackupConfig dir = BackupConfig { backupDir = dir, backupUseHardlinks = True }

data BackupState = BackupState
    { ignoreFormats   :: [FileFormat]
    , terminal        :: Terminal
    , stats           :: Stats
    }

newtype BackupRO a = BackupRO { unBackupRO :: ReaderT BackupConfig IO a }
    deriving (MonadReader BackupConfig, MonadIO, Monad, Functor, Applicative)

newtype Backup a = Backup { unBackup :: StateT BackupState (ReaderT BackupConfig IO) a }
    deriving (MonadReader BackupConfig, MonadState BackupState, MonadIO, Monad, Functor, Applicative)

runBackup :: Backup a -> BackupState -> BackupConfig -> IO (a, BackupState)
runBackup f bs cfg = runReaderT (runStateT (unBackup f) bs) cfg

runBackupRO :: BackupRO a -> BackupConfig -> IO a
runBackupRO f bs = runReaderT (unBackupRO f) bs

modifyStats :: (Stats -> Stats) -> Backup ()
modifyStats f = modify (\st -> st { stats = f (stats st) })

incErrors :: Backup ()
incErrors = modifyStats (\st -> st { statsErrors = statsErrors st + 1})
incDups :: Backup ()
incDups = modifyStats (\st -> st { statsDups = statsDups st + 1})
incSkipped :: Backup ()
incSkipped = modifyStats (\st -> st { statsSkipped = statsSkipped st + 1})
incProcessed :: Backup ()
incProcessed = modifyStats (\st -> st { statsProcessed = statsProcessed st + 1})
addBytes :: Word64 -> Backup ()
addBytes n = modifyStats (\st -> st { statsAddedBytes = statsAddedBytes st + n})
