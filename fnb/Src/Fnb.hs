{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Monad
import System.Environment
import System.Directory
import System.FilePath
import System.Posix.Files
import Data.FileFormat
import Control.Exception (handle, SomeException)

data Cfg = CfgIgnore

parseFnbFile d =
    case lines d of
        "ignore":[] -> Just CfgIgnore
        _           -> Nothing

main = do
    home <- getEnv "HOME"
    processDir home
  where processDir dir = do
            isFnb  <- doesFileExist (dir </> ".fnb")
            dirCfg <-
                    if isFnb
                        then parseFnbFile <$> readFile (dir </> ".fnb")
                        else return Nothing
            case dirCfg of
                Just CfgIgnore -> return ()
                Nothing -> do
                    isChronofs    <- doesFileExist (dir </> ".chronofs")
                    isGit         <- doesDirectoryExist (dir </> ".git")
                    if isChronofs
                        then return ()
                        else do if isGit
                                    then processGitDir dir
                                    else processNormalDir dir

        processGitDir dir = do
            putStrLn ("found a git dir at " ++ show dir)
        processNormalDir dir = do
            putStrLn ("process dir " ++ show dir)
            ents <- filter (not . flip elem [".", ".."]) <$> getDirectoryContents dir
            forM_ ents $ \ent -> do
                let ff = dir </> ent
                st           <- getSymbolicLinkStatus ff
                isAccessible <- handle (\(_::SomeException) -> return False) (fileAccess ff True False (isDirectory st))
                when isAccessible $ do
                    if isRegularFile st
                        then processFile ff
                        else if isDirectory st
                                then (if ent `elem` [ ".chronofs", ".svn", ".git" ] then return () else processDir ff)
                                else return ()
        processFile file = do
            format <- getFileformat file
            --putStrLn ("file : " ++ show file ++ " has format: " ++ show format)
            return ()
