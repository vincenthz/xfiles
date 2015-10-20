{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.List
import           Data.FileFormat
import           System.Directory
import           System.Directory.Traverse
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
import           "cryptonite" Crypto.Hash

data Configuration = Configuration
    { shredDestination    :: FilePath
    , shredDestinationLog :: FilePath
    , badExtensions       :: [String]
    } deriving (Show,Eq)

digestFile :: FilePath -> IO (Digest SHA256)
digestFile f =
    hashlazy <$> L.readFile f :: IO (Digest SHA256)

inDirectory :: FilePath -> String -> FilePath -> [String] -> IO ()
inDirectory dir cmdName cmd cmdArgs = do
    saved <- getCurrentDirectory
    setCurrentDirectory dir
    putStrLn ("===> " ++ cmdName)
    exitCode <- rawSystem cmd cmdArgs
    if exitCode /= ExitSuccess
        then error ("cmd: " ++ cmd ++ " " ++ show cmdArgs ++ " failed with code: " ++ show exitCode)
        else setCurrentDirectory saved >> return ()

logNew :: Configuration -> FilePath -> IO Handle
logNew cfg rdir = do
    let logDir = shredDestinationLog cfg
        baseFile = dirToLogName rdir
    exist <- doesFileExist (logDir </> baseFile)
    logFacility <- if exist
                    then findFree logDir baseFile 1
                    else return (logDir </> baseFile)
    logHandle <- openFile logFacility WriteMode
    return logHandle
  where dirToLogName [] = []
        dirToLogName (x:xs)
            | x == '/'  = '#' : dirToLogName xs
            | otherwise = x   : dirToLogName xs
        findFree :: FilePath -> FilePath -> Int -> IO FilePath
        findFree dir f i = do
            let file = dir </> (f ++ "." ++ show i)
            e <- doesFileExist file
            if e
                then findFree dir f (i+1)
                else return file


logAppend logFacility file digest = do
    hPutStrLn logFacility (file ++ " == " ++ show digest)
    return ()

logFlush _ logFacility _ = do
    hClose logFacility
    return ()

shred cfg logFacility file = do
    digest <- digestFile file
    -- add the file somewhere
    let dest = shredDestination cfg
        (digestDir,digestFileName) = splitAt 2 $ show digest
    let destDir  = dest </> digestDir
        destFile = destDir </> digestFileName
    createDirectoryIfMissing True destDir
    alreadyExist <- doesFileExist destFile
    unless alreadyExist $ do
        putStrLn ("adding: " ++ file ++ " at : " ++ destFile)
        copyFile file destFile
        logAppend logFacility file digest

safeShred cfg dir = do
    logFacility <- logNew cfg dir
    dirTraverse_ dir (fileCallback logFacility) (dirCallback logFacility)
    logFlush cfg logFacility dir
  where fileCallback logFacility f
            | isRubbishByName f = return ()
            | otherwise = do
                isRubbish <- isContentRubbish cfg f
                unless isRubbish $ shred cfg logFacility f
                return ()
        dirCallback logFacility dirPath
            | isSuffixOf ".git" dirPath = do
                let outName = takeFileName dirPath ++ ".tar.gz"
                inDirectory (takeDirectory dirPath) ("taring git repo " ++ (takeFileName dirPath))
                        "tar" ["cvzf", outName, takeFileName dirPath]
                shred cfg logFacility (dirPath ++ ".tar.gz")
                return False
            | otherwise = do
                return True

        isRubbishByName f
            | hasExt    = (drop 1 ext) `elem` badExtensions cfg
            | otherwise = False
          where ext    = takeExtension f
                hasExt = not (null ext) && head ext == '.'
        isContentRubbish _ f = do
            sz <- withFile f ReadMode hFileSize
            if sz > 500000
                then do format <- getFileformat f
                        if format `elem` [FT_ELF,FT_AR,FT_MACH_O]
                            then return True
                            else return False
                else return False

main = do
    home <- getHomeDirectory
    let shredDest = home </> ".shred"
        shredDestLog = shredDest </> "log"
    
    createDirectoryIfMissing True shredDest
    createDirectoryIfMissing True shredDestLog

    args <- getArgs
    -- FIXME do arguments parsing here
    let dirs = args
    let cfg = Configuration shredDest shredDestLog ["o","dyn_o","hi","so","dylib","a","dyn_hi"]
    mapM_ (safeShred cfg) dirs
