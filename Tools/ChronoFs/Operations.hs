{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Tools.ChronoFs.Operations
    ( readMeta
    , readMeta_
    , readBackup
    , readBackup_
    , resolvePath
    , pathData
    , pathMeta
    , pathBackup
    , removeData
    , writeEntAsHash
    -- * filesystem operations
    , hardlink
    , symlink
    , getFileformat
    ) where

import           Tools.ChronoFs.Types
import           Tools.ChronoFs.Utils
import           Tools.ChronoFs.Monad
import           Tools.ChronoFs.Marshall

import           Data.FileFormat (getFileformatFrom, FileFormat)

import           Control.Applicative

import           Data.String
import           Data.List
import           System.Posix.Files.ByteString hiding (isDirectory)
import           System.IO
import           System.Directory
import           System.FilePath

import           Crypto.Hash hiding (hash)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Char8 as BC

import           Control.Monad.State
import           Control.Monad.Reader

type BackupName = String

-- | Copy data from a callback to the file representing its content (by hash)
--
-- Create a new temporary file that is going to receive data from a callback function.
--
-- when this callback function is done, the temporary file is renamed to the hash(content)
-- filename in the @destDir dir.
writeAsHash :: FilePath
            -> FilePath
            -> ((Context HashT -> BC.ByteString -> IO (Context HashT))
                -> Context HashT -> IO (Context HashT))
            -> IO Hash
writeAsHash tmpFile destDir f = do
    hash <- withFile tmpFile WriteMode $ \handl -> do
        !r <- f (\ctx b -> B.hPut handl b >> return (hashUpdate ctx b)) hashInit
        return $! Hash $! hashFinalize r
    let hashName  = hexHash hash
        hashDir   = take 2 hashName
        hashFile  = drop 2 $ hashName
        finalDir  = destDir </> fromString hashDir
        finalName = finalDir </> fromString hashFile
    createDirectoryIfMissing True finalDir
    rename (UTF8.fromString tmpFile) (UTF8.fromString finalName)
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
  where augment bdir = bdir </> "backup" </> name

-- | read the meta information associated with a hash
readMeta :: (Functor f, MonadReader BackupConfig f, MonadIO f)
         => Hash -> f (Either String [Ent])
readMeta hash = unmarshallEnts <$> (pathMeta hash >>= liftIO . B.readFile)

readMeta_ :: (Functor f, MonadReader BackupConfig f, MonadIO f)
          => Hash -> f [Ent]
readMeta_ hash = either (\e -> error $ "read meta failed: " ++ e) id <$> readMeta hash

-- | Read a backup file
readBackup :: (Functor m, MonadIO m, MonadReader BackupConfig m)
           => BackupName -> m (Either String Hash)
readBackup name = do
    backupFile <- pathBackup name
    content    <- liftIO (B.readFile backupFile)
    return $ hashHexAsBs (head $ BC.lines content) -- FIXME make it headless

readBackup_ :: (Functor m, MonadIO m, MonadReader BackupConfig m)
            => BackupName -> m Hash
readBackup_ name = either (\e -> error $ "read backup failed: " ++ e) id <$> readBackup name

getBDir :: MonadReader BackupConfig m => m FilePath
getBDir = asks backupDir

-- | Resolve the path recursively from the root hash
-- and if found, return the hash associated with the latest element of the path asked
resolvePath :: (Functor m, MonadIO m, MonadReader BackupConfig m)
            => Hash -> FilePath -> m (Either String Hash)
resolvePath rootHash rootPath = loop rootHash (splitDirectories rootPath)
  where loop hash []     = return $ Right hash
        loop hash (p:ps)
            | p == "/"   = loop hash ps
            | otherwise  = do
                --liftIO $ putStrLn ("resolvePath: " ++ encodeString p ++ " " ++ show ps)
                ents <- readMeta_ hash
                case find (\e -> entName e == p) ents of
                    Nothing  -> return $ Left ("entity " ++ show p ++ " not found in " ++ show rootPath)
                    Just ent ->
                        case entHash ent of
                            ContentLink _ -> return $ Left ("entity " ++ show p ++ " is a link")
                            ContentHash h -> loop h ps

getFileformat :: FilePath -> IO FileFormat
getFileformat ent = withFile ent ReadMode (\h -> getFileformatFrom <$> B.hGet h 512)

hardlink :: FilePath -> FilePath -> IO ()
hardlink origName symbolicName = createLink (UTF8.fromString origName) (UTF8.fromString symbolicName)

symlink :: FilePath -> FilePath -> IO ()
symlink origName symbolicName = createSymbolicLink (UTF8.fromString origName) (UTF8.fromString symbolicName)
