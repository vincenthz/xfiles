-- |
-- Module      : System.Directory.Traverse
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Simple helper to traverse through directories and files.
--
{-# LANGUAGE ScopedTypeVariables #-}
module System.Directory.Traverse
    ( dirTraverse
    , dirTraverse_
    ) where

import System.Directory
import System.FilePath
import System.Posix.Files
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative ((<$>))
import Control.Exception

-- | Callback on a file name.
type FileCallback io accumulator = accumulator -> FilePath -> io accumulator

-- | Callback on a directory name.
--
-- If the function returns True, then the directory itself is traversed
type DirCallback io accumulator = accumulator -> FilePath -> io (Bool, accumulator)

-- | Traverse directories and files starting from the @rootDir
dirTraverse :: MonadIO io
            => FilePath
            -> FileCallback io a
            -> DirCallback io a
            -> a
            -> io a
dirTraverse rootDir fFile fDir rootAcc = loop rootAcc rootDir
  where loop a dir = do
            content <- liftIO $ try $ getDir dir
            case content of
                Left (_ :: SomeException) -> return a
                Right l                   -> foldM (processEnt dir) a l
        processEnt dir a ent = do
            let fp = dir </> ent
            stat <- liftIO $ getSymbolicLinkStatus fp
            case (isDirectory stat, isRegularFile stat) of
                (True,_)     -> do (process,a') <- fDir a fp
                                   if process
                                      then loop a' fp
                                      else return a'
                (False,True)  -> fFile a fp
                (False,False) -> return a
        getDir dir = filter (not . flip elem [".",".."]) <$> getDirectoryContents dir

-- | Just like 'dirTraverse' but doesn't give the ability to accumulate anything.
dirTraverse_ :: MonadIO io
             => FilePath
             -> (FilePath -> io ())
             -> (FilePath -> io Bool)
             -> io ()
dirTraverse_ rootDir fFile fDir =
    dirTraverse rootDir (\_ -> fFile) (\_ fp -> (\b -> (b, ())) <$> fDir fp) ()
