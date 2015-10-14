module Main where

import           Data.List
import           Data.FileFormat
import           System.Directory
import           System.Directory.Traverse
import           System.Environment

main = do
    argDirs <- getArgs
    let dirs = if null argDirs then ["."] else argDirs
    mapM_ (\d -> dirTraverse_ d fileCallback dirCallback) dirs
  where
        -- skip git repository
        dirCallback dirPath
            | "/.git" `isSuffixOf` dirPath = do
                return False
            | otherwise = do
                return True

        fileCallback f = do
            fo <- getFileformat f
            putStrLn ("" ++ f ++ " : " ++ show fo)
            return ()            
