{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Tools.ChronoFs.Operations where

import Tools.ChronoFs.Types
import Tools.ChronoFs.Utils
import Tools.ChronoFs.Monad
import Tools.ChronoFs.Marshall

import Data.FileFormat (getFileformatFrom, FileFormat)

import Filesystem (createDirectory, readFile, removeFile, IOMode(..), withFile)
import Filesystem.Path.CurrentOS

import Control.Applicative

import Data.String
import Data.List
import System.Posix.Files.ByteString hiding (isDirectory)

import qualified Crypto.Hash.SHA512 as SHA512
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as Base16

import Control.Monad.State
import Control.Monad.Reader

import Prelude hiding (FilePath, writeFile, readFile)

type BackupName = String

writeAsHash :: FilePath
            -> FilePath
            -> ((SHA512.Ctx -> BC.ByteString -> IO SHA512.Ctx)
            -> SHA512.Ctx -> IO SHA512.Ctx)
            -> IO Hash
writeAsHash tmpFile destDir f = do
    hash <- withFile tmpFile WriteMode $ \handl -> do
        !r <- f (\ctx b -> B.hPut handl b >> return (SHA512.update ctx b)) SHA512.init
        return $! Hash $! SHA512.finalize r
    let hashName  = hexHash hash
        hashDir   = take 2 hashName
        hashFile  = drop 2 $ hashName
        finalDir  = destDir </> fromString hashDir
        finalName = finalDir </> fromString hashFile
    createDirectory True finalDir
    rename (encode tmpFile) (encode finalName)
    return hash

-- write a list of entities as a new meta file
writeEntAsHash :: (MonadReader BackupConfig m, MonadIO m) => [Ent] -> m Hash
writeEntAsHash ents = getBDir >>= \bdir ->
    liftIO $ writeAsHash (bdir </> "tmp" </> "ent") (bdir </> "meta") wr
  where wr write initCtx =
            foldM (\hashCtx ent -> write hashCtx (marshallEnt ent)) initCtx ents

-- | return path to the data
pathData :: (Functor f, MonadReader BackupConfig f) => Hash -> f FilePath
pathData hash = augment <$> getBDir
  where augment bdir = bdir </> "data" </> fromString hashDir </> fromString hashDir2 </> fromString hashFile
        hashName  = hexHash hash
        hashDir   = take 2 hashName
        hashDir2  = take 2 $ drop 2 $ hashName
        hashFile  = drop 4 $ hashName

-- | remove some data
removeData :: (Functor f, MonadReader BackupConfig f, Monad f, MonadIO f) => Hash -> f ()
removeData hash = pathData hash >>= liftIO . removeFile

-- | get the full path to a meta file
pathMeta :: (Functor f, MonadReader BackupConfig f) => Hash -> f FilePath
pathMeta hash = augment <$> getBDir
  where augment bdir = bdir </> "meta" </> fromString hashDir </> fromString hashFile
        hashName  = hexHash hash
        hashDir   = take 2 hashName
        hashFile  = drop 2 $ hashName

-- | Return the path to a backup called @name
pathBackup :: (Functor f, MonadReader BackupConfig f) => BackupName -> f FilePath
pathBackup name = augment <$> getBDir
  where augment bdir = bdir </> "backup" </> decodeString name

-- | read the meta information associated with a hash
readMeta :: (Functor f, MonadReader BackupConfig f, MonadIO f)
         => Hash -> f (Either String [Ent])
readMeta hash = unmarshallEnts <$> (pathMeta hash >>= liftIO . readFile)

readBackup :: (Functor m, MonadIO m, MonadReader BackupConfig m)
           => BackupName -> m Hash
readBackup name = do
    backupFile <- pathBackup name
    Hash . fst . Base16.decode . head . BC.lines <$> liftIO (readFile backupFile)

getBDir :: MonadReader BackupConfig m => m FilePath
getBDir = asks backupDir

-- hash of the base directory
resolvePath :: (Functor m, MonadIO m, MonadReader BackupConfig m)
            => Hash -> FilePath -> m (Either String Hash)
resolvePath rootHash rootPath = loop rootHash (splitDirectories rootPath)
  where loop hash []     = return $ Right hash
        loop hash (p:ps)
            | encodeString p == "/" = loop hash ps
            | otherwise             = do
                --liftIO $ putStrLn ("resolvePath: " ++ encodeString p ++ " " ++ show ps)
                ents <- either error id <$> readMeta hash
                case find (\e -> entName e == p) ents of
                    Nothing  -> return $ Left ("entity " ++ show p ++ " not found in " ++ show rootPath) 
                    Just ent -> loop (entHash ent) ps

getFileformat :: FilePath -> IO FileFormat
getFileformat ent = withFile ent ReadMode (\h -> getFileformatFrom <$> B.hGet h 512)

hardlink :: FilePath -> FilePath -> IO ()
hardlink origName symbolicName = createLink (encode origName) (encode symbolicName)

symlink :: FilePath -> FilePath -> IO ()
symlink origName symbolicName = createSymbolicLink (encode origName) (encode symbolicName)
