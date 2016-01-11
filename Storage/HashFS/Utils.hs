module Storage.HashFS.Utils where

import qualified Control.Exception as E

import           Data.List (isPrefixOf)
import           System.Directory
import           System.FilePath

catchIO :: IO a -> IO (Maybe a)
catchIO f = E.catch (Just `fmap` f) toNothing
  where toNothing :: E.IOException -> IO (Maybe a)
        toNothing _ = return Nothing

normalizeLocalPath :: String -> IO FilePath
normalizeLocalPath p
    | "~/" `isPrefixOf` p = do
        home <- getHomeDirectory
        return (home </> drop 2 p)
    | otherwise = return p

isEmptyDirectory :: FilePath -> IO Bool
isEmptyDirectory c = do
    isNull <- null . filter metaDirs <$> getDirectoryContents c
    return isNull
  where metaDirs = not . flip elem [".", ".."]
