module Main where

import           Data.List
import           Data.FileFormat
import           System.Directory
import           System.Directory.Traverse
import           System.Environment

main = do
    args <- getArgs
    dirTraverse_ "." (fileCallback log) (dirCallback log)
  where
        -- skip git repository
        dirCallback log dirPath
            | ".git" == dirPath = do
                return False
            | otherwise = do
                return True

        fileCallback log f = do
            putStrLn ("" ++ f)
            return ()            
