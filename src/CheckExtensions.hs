module Main where

import           Data.List
import           Data.FileFormat
import           System.Directory
import           System.Directory.Traverse
import           System.Environment
import           Data.FileFormat

main = do
    args <- getArgs
    dirTraverse_ "." (fileCallback log) (dirCallback log)
  where
        -- skip git repository
        dirCallback log dirPath
            | "/.git" `isSuffixOf` dirPath = do
                return False
            | otherwise = do
                return True

        fileCallback log f = do
            fo <- getFileformat f
            putStrLn ("" ++ f ++ " : " ++ show fo)
            return ()            
