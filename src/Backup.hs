{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.Directory hiding (listDirectory)
import System.FilePath
import Prelude
import "mtl" Control.Monad.State
import Control.Monad.Reader

import Control.Applicative
import Data.Default.Class
import Data.String
import Data.Time.Clock.POSIX
import System.Posix.Files.ByteString hiding (isDirectory)
import System.Environment (getArgs)
import System.Exit

import Data.List
import Data.Maybe

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.UTF8 as UTF8

import System.Console.GetOpt
import Console.Display

import Tools.ChronoFs

listDirectory :: ExcludeQuery -> FilePath -> IO [FilePath]
listDirectory isExcluded dir = toPath <$> getDirectoryContents dir
  where
    toPath []        = []
    toPath (".":xs)  = toPath xs
    toPath ("..":xs) = toPath xs
    toPath (x:xs)
        | isExcluded dir x = toPath xs
        | otherwise        = (dir </> x) : toPath xs

{-
data Exclude =
      ExcludeHome String
    | ExcludeAbs String
    deriving (Show,Eq)
    -}

excludeHome :: [String]
excludeHome =
    [".cabal", ".atom", ".shred", ".Trash", ".stack", ".ghc-mod", ".cache", ".gem"
    , ".npm", ".npm-install", ".z", ".bur", ".local", ".stackage-update", ".haste", ".gitcache", ".docker"
    ,"cache", "Library", "Downloads", "Movies"]

type ExcludeQuery = FilePath -> String -> Bool

excludeQuery :: FilePath -> ExcludeQuery
excludeQuery homeDir = \dir ent ->
    if dir == homeDir
        then ent `elem` excludeHome
        else False

noExcludeQuery :: ExcludeQuery
noExcludeQuery = \_ _ -> False

cmdBackup :: [AllOpt] -> String -> IO ()
cmdBackup opts backupName = do
    bdir <- getBackupDir opts
    home <- getHomeDirectory
    term <- displayInit
    let cfg = BackupConfig { backupDir = bdir, backupUseHardlinks = True }
        bs  = BackupState
                { ignoreFormats = defaultBadFileformats
                , terminal      = term
                , stats         = def
                }
    let isExcluded = excludeQuery home
    initialTime <- getPOSIXTime
    (ment,finalState) <- runBackup (initCheck >> loop isExcluded bdir home) bs cfg
    finalTime   <- getPOSIXTime
    case ment of
        Nothing  -> putStrLn "no archiving done"
        Just ent -> do
            (!hash,_) <- runBackup (writeEntAsHash [ent]) bs cfg
            putStrLn $ show $ stats finalState
            let commitFilename = UTF8.toString $ B.concat [BC.pack backupName, "-"
                                                         ,(BC.pack $ show (truncate initialTime :: Integer)), "-"
                                                         ,(BC.pack $ show (truncate finalTime :: Integer))]
            let commitFilepath = bdir </> "backup" </> commitFilename
            B.writeFile commitFilepath (hexHashAsBs hash `B.append` "\n")
            putStrLn $ (commitFilename ++ " written in " ++ (show (finalTime - initialTime)))
    where
          initCheck = do bdir <- asks backupDir
                         mapM_ (liftIO . createDirectoryIfMissing True) [ bdir, bdir </> "tmp", bdir </> "data", bdir </> "meta", bdir </> "backup" ]
          loop :: ExcludeQuery -> FilePath -> FilePath -> Backup (Maybe Ent)
          loop exclude skipDir dir
            | dir == skipDir = return Nothing
            | otherwise      = do
                verbose2 ("entering " ++ show dir)
                m <- runIO (listDirectory exclude dir) (return []) (return)
                allEnts <- catMaybes <$> mapM (processEnt exclude skipDir) m
                !hash <- writeEntAsHash allEnts
                !fs <- liftIO $ getSymbolicLinkStatus (UTF8.fromString dir)
                return $ Just $ Ent { entType  = EntDir
                                    , entPerms = fileMode fs
                                    , entMTime = realToFrac $ modificationTime fs
                                    , entCTime = realToFrac $ statusChangeTime fs
                                    , entHash  = ContentHash hash
                                    , entName  = takeFileName dir
                                    }
          processEnt exclude skipDir ent = do
            fm <- liftIO (either error id <$> getFileMetas ent)
            if fileMetaType fm == Directory
                then loop exclude skipDir ent
                else runIO (gatherFileMeta ent) (return Nothing) (processFile ent fm)
          processFile ent fm (ty,hash)
                | fileMetaType fm == SymbolicLink = doSymlink
                | fileMetaType fm == RegularFile  = doFile
                | otherwise                       = return Nothing
                where doSymlink = do lnk <- liftIO $ readSymbolicLink (UTF8.fromString ent)
                                     return $ Just $ Ent { entType = EntLink
                                                         , entPerms = fileMetaMode fm
                                                         , entMTime = realToFrac $ fileMetaModTime fm
                                                         , entCTime = realToFrac $ fileMetaChTime fm
                                                         , entHash  = ContentLink lnk
                                                         , entName  = takeFileName ent
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
                            exists <- liftIO (doesFileExist finalName)
                            if exists
                                then incDups
                                else do
                                        addBytes (fromIntegral $ fileMetaSize fm)
                                        useHard <- asks backupUseHardlinks
                                        runIO_ (createDirectoryIfMissing True finalDir1)
                                        runIO_ (createDirectoryIfMissing True finalDir)
                                        if useHard
                                            then runIO_ (hardlink ent finalName)
                                            else runIO_ (copyFile ent tmpName >> rename (UTF8.fromString tmpName) (UTF8.fromString finalName))
                            return $ Just $ Ent { entType  = EntFile
                                                , entPerms = fileMetaMode fm
                                                , entMTime = realToFrac $ fileMetaModTime fm
                                                , entCTime = realToFrac $ fileMetaChTime fm
                                                , entHash  = ContentHash hash
                                                , entName  = takeFileName ent
                                                }
          addOne = incProcessed >> showStat False
          gatherFileMeta ent = do
            !ty <- getFileformat ent
            let h = getFileHash ent
            return (ty,h)

          verbose2 s = printTerminalLn Green s

cmdList :: [AllOpt] -> IO ()
cmdList opts = do
    bdir    <- getBackupDir opts
    backups <- listDirectory noExcludeQuery (bdir </> "backup")
    mapM_ (putStrLn . takeFileName) backups

cmdListName :: [AllOpt] -> String -> IO ()
cmdListName = undefined

cmdGetMeta :: [AllOpt] -> String -> IO ()
cmdGetMeta opts hashName = do
    bdir <- getBackupDir opts
    --runBackupRO (initCheck >> loop (backupDir bs) home) (defaultBackupConfig bdir)
    flip runBackupRO (defaultBackupConfig bdir) $ do
        ents <- readMeta_ (either error id $ hashHex hashName)
        liftIO $ mapM_ (printEnt Raw) ents

cmdGet :: [AllOpt] -> String -> IO ()
cmdGet opts name = do
    bdir <- getBackupDir opts
    flip runBackupRO (defaultBackupConfig bdir) $ do
        hash <- readBackup_ name
        ents <- readMeta_ hash
        liftIO $ mapM_ (printEnt Raw) ents

data EntPrint = Raw | Pretty

printEnt :: EntPrint -> Ent -> IO ()
printEnt Raw ent = do
    putStrLn ("filename " ++ entName ent)
    case entHash ent of
        ContentHash h   -> putStrLn ("hash " ++ hexHash h)
        ContentLink lnk -> putStrLn ("link " ++ show lnk)
    putStrLn ("perms " ++ (show $ entPerms ent))
    putStrLn ("mtime " ++ (show $ entMTime ent))
    putStrLn ("ctime " ++ (show $ entMTime ent))

printEnt Pretty ent = do
    putStrLn ("+ " ++ entName ent ++ marker ++ " (" ++ content ++ ")")
  where
        content = case entHash ent of
                    ContentLink lnk -> show lnk
                    ContentHash h   -> showSmall (hexHash h)
        showSmall = take 16
        marker = case entType ent of
                        EntLink  -> "@"
                        EntFile  -> ""
                        EntDir   -> "/"

getRootEnt :: [Ent] -> [FilePath] -> BackupRO (Ent, [Ent])
getRootEnt es l = doGet es l
    where doGet _    []     = error "no path"
          doGet ents (x:[]) =
              case find (\e -> entName e == x) ents of
                          Nothing -> error "couldn't find"
                          Just e  -> do
                              childEnts <- readMeta_ (getHash e)
                              return (e,childEnts)
          doGet ents (x:xs) =
              case find (\e -> entName e == x) ents of
                          Nothing -> error "couldn't find"
                          Just e  -> do
                              childEnts <- readMeta_ (getHash e)
                              doGet childEnts xs

          getHash e = case entHash e of
                            ContentLink lnk -> error ("got link: " ++ show lnk)
                            ContentHash h   -> h

cmdShow :: [AllOpt] -> String -> Maybe FilePath -> IO ()
cmdShow opts name Nothing = do
    bdir <- getBackupDir opts
    flip runBackupRO (defaultBackupConfig bdir) $ do
        hash <- readBackup_ name
        ents <- readMeta_ hash
        liftIO $ mapM_ (printEnt Pretty) ents
cmdShow opts name (Just dir)
    | isAbsolute dir = error "source path cannot be absolute"
    | otherwise      = getBackupDir opts >>= runBackupRO doShow . defaultBackupConfig
        where doShow = do
                    hash <- readBackup_ name
                    ents <- readMeta_ hash

                    let paths = splitDirectories dir
                    (rootEnt,children) <- getRootEnt ents paths
                    liftIO $ printEnt Pretty rootEnt
                    liftIO $ mapM_ (printEnt Pretty) children

cmdDu :: [AllOpt] -> t -> FilePath -> IO b
cmdDu opts _name dir
    | isAbsolute dir = error "source path cannot be absolute"
    | otherwise      = getBackupDir opts >>= runBackupRO doDu . defaultBackupConfig
        where doDu = do
                    --hash <- readBackup_ name
                    --ents <- readMeta_ hash
                    error "du is not implemented"

cmdRestore :: [AllOpt] -> String -> FilePath -> FilePath -> IO ()
cmdRestore opts name rootDir dirTo
    | isAbsolute rootDir = error "source path cannot be absolute"
    | otherwise          = getBackupDir opts >>= runBackupRO doRestore . defaultBackupConfig
  where doRestore = do
            hash <- readBackup_ name
            ents <- readMeta_ hash
            restoreDir ents rootDir
          where -- restore a directory of stuff
                restoreDir ents dir = do
                    let paths = splitDirectories dir
                    (_rootEnt,children) <- getRootEnt ents paths

                    forM_ children $ \child -> do
                        case entType child of
                            EntDir -> restoreDir ents (dir </> entName child)
                            _      ->
                                case entHash child of
                                    ContentLink _ -> -- skip link
                                        return ()
                                    ContentHash h -> do
                                        liftIO $ printEnt Pretty child
                                        let n = entName child
                                        p <- pathData h
                                        liftIO $ symlink p (dirTo </> n)
                        return ()

allOpts :: [OptDescr AllOpt]
allOpts =
    [ Option ['u'] ["user"] (NoArg UserInstall) "use the user installation"
    , Option ['p'] ["path"] (ReqArg ExplicitInstall "path") "use an explicit path"
    ]

main :: IO ()
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
