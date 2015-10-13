{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Filesystem (getHomeDirectory, createDirectory, isFile, copyFile, writeFile, listDirectory)
import Filesystem.Path
import Filesystem.Path.Rules
import Prelude hiding (FilePath, writeFile, readFile)
import "mtl" Control.Monad.State
import Control.Monad.Reader

import Control.Applicative
import Data.Default.Class
import Data.String
import Data.Time.Clock.POSIX
import System.Posix.Files.ByteString hiding (isDirectory)
--import System.Posix.Env.ByteString hiding (getArgs)
import System.Environment (getArgs)
import System.Exit

import Data.List
import Data.Maybe

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as Base16

import System.Console.GetOpt
import System.Console.Terminfo

import Tools.ChronoFs.Types
import Tools.ChronoFs.Monad
import Tools.ChronoFs.Display
import Tools.ChronoFs.Utils
import Tools.ChronoFs.Operations
import Tools.ChronoFs.Config

cmdBackup opts backupName = do
    bdir <- getBackupDir opts
    home <- getHomeDirectory
    term <- setupTermFromEnv
    let cfg = BackupConfig { backupDir = bdir, backupUseHardlinks = True }
        bs  = BackupState
                { ignoreFormats= defaultBadFileformats
                , terminal     = term
                , stats        = def
                }
    initialTime <- getPOSIXTime
    (ment,finalState) <- runBackup (initCheck >> loop bdir home) bs cfg
    finalTime   <- getPOSIXTime
    case ment of
        Nothing  -> putStrLn "no archiving done"
        Just ent -> do
            (!hash,_) <- runBackup (writeEntAsHash [ent]) bs cfg
            putStrLn $ show $ stats finalState
            let commitFilename = decode posix $ B.concat [BC.pack backupName, "-"
                                                         ,(BC.pack $ show (truncate initialTime :: Integer)), "-"
                                                         ,(BC.pack $ show (truncate finalTime :: Integer))]
            let commitFilepath = bdir </> "backup" </> commitFilename
            writeFile commitFilepath (hexHashAsBs hash `B.append` "\n")
            putStrLn $ (encodeString posix commitFilename ++ " written in " ++ (show (finalTime - initialTime)))
    where 
          initCheck = do bdir <- asks backupDir
                         mapM_ (liftIO . createDirectory True) [ bdir, bdir </> "tmp", bdir </> "data", bdir </> "meta", bdir </> "backup" ]
          loop :: FilePath -> FilePath -> Backup (Maybe Ent)
          loop skipDir dir
            | dir == skipDir = return Nothing
            | otherwise      = do
                verbose2 ("entering " ++ show dir)
                m <- runIO (listDirectory dir) (return []) (return)
                allEnts <- catMaybes <$> mapM (processEnt skipDir) m
                !hash <- writeEntAsHash allEnts
                !fs <- liftIO $ getSymbolicLinkStatus (encode posix dir)
                return $ Just $ Ent { entType  = EntDir
                                    , entPerms = fileMode fs
                                    , entMTime = realToFrac $ modificationTime fs
                                    , entCTime = realToFrac $ statusChangeTime fs
                                    , entHash  = hash
                                    , entName  = filename dir
                                    }
          processEnt skipDir ent = do
            fm <- liftIO (either error id <$> getFileMetas ent)
            if fileMetaType fm == Directory
                then loop skipDir ent
                else runIO (gatherFileMeta ent) (return Nothing) (processFile ent fm)
          processFile ent fm (ty,hash)
                | fileMetaType fm == SymbolicLink = doSymlink
                | fileMetaType fm == RegularFile  = doFile
                | otherwise                       = return Nothing
                where doSymlink = do lnk <- liftIO $ readSymbolicLink (encode posix ent)
                                     return $ Just $ Ent { entType = EntLink
                                                         , entPerms = fileMetaMode fm
                                                         , entMTime = realToFrac $ fileMetaModTime fm
                                                         , entCTime = realToFrac $ fileMetaChTime fm
                                                         , entHash  = Hash lnk
                                                         , entName  = filename ent
                                                         }
                      doFile = addOne >> processFileNow ent fm (ty,hash)

          processFileNow ent fm (ty,hashio) = do
                ignore <- gets ignoreFormats
                if ty `elem` ignore
                    then incSkipped >> return Nothing
                    else do
                            hash <- liftIO hashio
                            bdir <- asks backupDir
                            let hashName  = hexHash hash
                                hashDir   = take 2 hashName
                                hashDir2  = take 2 $ drop 2 $ hashName
                                hashFile  = drop 4 $ hashName
                                tmpName   = bdir </> "tmp" </> fromString (hexHash hash)
                                finalDir1 = bdir </> "data" </> fromString hashDir
                                finalDir  = finalDir1 </> fromString hashDir2
                                finalName = finalDir </> fromString hashFile
                            exists <- liftIO (isFile finalName)
                            if exists
                                then incDups
                                else do
                                        addBytes (fromIntegral $ fileMetaSize fm)
                                        useHard <- asks backupUseHardlinks
                                        runIO_ (createDirectory True finalDir1)
                                        runIO_ (createDirectory True finalDir)
                                        if useHard
                                            then runIO_ (hardlink ent finalName)
                                            else runIO_ (copyFile ent tmpName >> rename (encode posix tmpName) (encode posix finalName))
                            return $ Just $ Ent { entType  = EntFile
                                                , entPerms = fileMetaMode fm
                                                , entMTime = realToFrac $ fileMetaModTime fm
                                                , entCTime = realToFrac $ fileMetaChTime fm
                                                , entHash  = hash
                                                , entName  = filename ent
                                                }
          addOne = incProcessed >> showStat False
          gatherFileMeta ent = do
            !ty <- getFileformat ent
            let h = getFileHash ent
            return (ty,h)

          verbose2 s = printTerminalLn Green s

cmdList opts = do
    bdir    <- getBackupDir opts
    backups <- listDirectory (bdir </> "backup")
    mapM_ (putStrLn . encodeString posix . filename) backups

cmdListName = undefined

cmdGetMeta opts hashName = do
    bdir <- getBackupDir opts
    --runBackupRO (initCheck >> loop (backupDir bs) home) (defaultBackupConfig bdir)
    flip runBackupRO (defaultBackupConfig bdir) $ do
        ents <- either error id <$> readMeta (Hash $ Base16.encode $ BC.pack $ hashName)
        liftIO $ mapM_ (printEnt Raw) ents

cmdGet opts name = do
    bdir <- getBackupDir opts
    flip runBackupRO (defaultBackupConfig bdir) $ do
        hash <- readBackup name
        ents <- either error id <$> readMeta hash
        liftIO $ mapM_ (printEnt Raw) ents

data EntPrint = Raw | Pretty

printEnt Raw ent = do
    putStrLn ("filename " ++ encodeString posix (entName ent))
    putStrLn ("hash " ++ (hexHash $ entHash ent))
    putStrLn ("perms " ++ (show $ entPerms ent))
    putStrLn ("mtime " ++ (show $ entMTime ent))
    putStrLn ("ctime " ++ (show $ entMTime ent))

printEnt Pretty ent = do
    putStrLn ("+ " ++ encodeString posix (entName ent) ++ marker ++ " (" ++ showSmall (hexHash $ entHash ent) ++ ")")
    where showSmall = take 16
          marker = case entType ent of
                        EntLink  -> "@"
                        EntFile  -> ""
                        EntDir   -> "/"

getRootEnt es l = doGet es l
    where doGet _    []     = error "no path"
          doGet ents (x:[]) =
              case find (\e -> entName e == x) ents of
                          Nothing -> error "couldn't find"
                          Just e  -> do
                              childEnts <- either error id <$> readMeta (entHash e)
                              return (e,childEnts)
          doGet ents (x:xs) =
              case find (\e -> entName e == x) ents of
                          Nothing -> error "couldn't find"
                          Just e  -> do
                              childEnts <- either error id <$> readMeta (entHash e)
                              doGet childEnts xs

cmdShow opts name Nothing = do
    bdir <- getBackupDir opts
    flip runBackupRO (defaultBackupConfig bdir) $ do
        hash <- readBackup name
        ents <- either error id <$> readMeta hash
        liftIO $ mapM_ (printEnt Pretty) ents
cmdShow opts name (Just (decodeString posix -> dir))
    | absolute dir = error "source path cannot be absolute"
    | otherwise    = getBackupDir opts >>= runBackupRO doShow . defaultBackupConfig
        where doShow = do
                    hash <- readBackup name
                    ents <- either error id <$> readMeta hash

                    let paths = splitDirectories dir
                    (rootEnt,children) <- getRootEnt ents paths
                    liftIO $ printEnt Pretty rootEnt
                    liftIO $ mapM_ (printEnt Pretty) children

cmdDu opts name (decodeString posix -> dir)
    | absolute dir = error "source path cannot be absolute"
    | otherwise    = getBackupDir opts >>= runBackupRO doDu . defaultBackupConfig
        where doDu = do
                    hash <- readBackup name
                    ents <- either error id <$> readMeta hash
                    undefined

cmdRestore opts name (decodeString posix -> rootDir) (decodeString posix -> dirTo)
    | absolute rootDir = error "source path cannot be absolute"
    | otherwise        = getBackupDir opts >>= runBackupRO doRestore . defaultBackupConfig
  where doRestore = do
                    hash <- readBackup name
                    ents <- either error id <$> readMeta hash
                    restoreDir ents rootDir
          where restoreDir ents dir = do
                    let paths = splitDirectories dir
                    (rootEnt,children) <- getRootEnt ents paths

                    forM_ children $ \child -> do
                        case entType child of
                            EntDir -> restoreDir ents (dir </> entName child)
                            _      -> do
                                liftIO $ printEnt Pretty child
                                let h = entHash child
                                    n = entName child
                                p <- pathData h
                                liftIO $ symlink p (dirTo </> n)
                        return ()

allOpts =
    [ Option ['u'] ["user"] (NoArg UserInstall) "use the user installation"
    , Option ['p'] ["path"] (ReqArg ExplicitInstall "path") "use an explicit path"
    ]

main = do
    rawArgs <- getArgs
    let (opts, args, errs) = getOpt RequireOrder allOpts rawArgs
    when (errs /= []) $ do
        putStrLn ("error: cmdline arguments unexpected, expecting subcommand")
        mapM_ putStrLn errs
        exitFailure

    case args of
        "run":name:[]      -> cmdBackup opts name
        "run":[]           -> usage
        "list":[]          -> cmdList opts
        "list":name:[]     -> cmdListName opts name
        "get":name:[]      -> cmdGet opts name
        "get-meta":hash:[] -> cmdGetMeta opts hash
        "show":name:[]     -> cmdShow opts name Nothing
        "show":name:dir:[] -> cmdShow opts name (Just dir)
        "du":name:dir:[]   -> cmdDu opts name dir
        "restore":name:dir:dir2:[] -> cmdRestore opts name dir dir2
        _                  -> usage
    where usage = do putStrLn "usage: chronofs <cmd>"
                     putStrLn "    run <name>       -- run a backup"
                     putStrLn "    list [name]      -- list backup"
                     putStrLn "    get <name>       -- get backup information"
                     putStrLn "    get-meta <hash>  -- get hash information"
                     putStrLn "    show <name> [dir]  -- show a backup file and directory (optional starting point)"
                     putStrLn "    du <name> [dir]  -- report disk usage"
                     putStrLn "    restore <name> <dir> <to-dir> -- restore a directory to a destination"
                     putStrLn ""
