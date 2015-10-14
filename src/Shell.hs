{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad
import qualified Control.Exception as E
--import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent.MVar

import System.Console.SimpleReadline
import System.Console.Terminfo
import System.Console.GetOpt

import System.Exit
import System.Environment
import Tools.ChronoFs
--import Tools.ChronoFs.Types
--import Tools.ChronoFs.Utils
--import Tools.ChronoFs.Config
--import Tools.ChronoFs.Monad
--import Tools.ChronoFs.Operations

import Data.List (find)
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import System.Locale

import ShellGlob

--import Filesystem.Path.CurrentOS (FilePath, decodeString, encodeString, parent, (</>))
import Filesystem.Path.CurrentOS
import Prelude hiding (FilePath)

type ErrorMsg = String

newtype CliM a = CliM { unCli :: StateT CliState IO a }
    deriving (Functor, Applicative, Monad, MonadState CliState, MonadIO)

runCliM :: (String, BackupConfig) -> CliM a -> IO a
runCliM (name, cfg) f = do
    term <- liftIO setupTermFromEnv
    (rh, re) <- flip runBackupRO cfg $ do
                    rootHash <- readBackup name
                    rootEnts <- either error id <$> readMeta rootHash
                    return (rootHash, rootEnts)
    evalStateT (unCli f) $ CliState
        { cliPath      = decodeString "/"
        , cliPathHash  = rh
        , cliPathEnts  = re
        , cliBackupCfg = cfg
        , cliTerm      = term
        }

catchCliM :: CliM a -> (E.SomeException -> IO a) -> CliM a
catchCliM f errF = do
    st <- get
    (a, st2) <- liftIO $ E.catch (runStateT (unCli f) st) (\exn -> errF exn >>= \b -> return (b, st))
    put st2
    return a

doBackup :: BackupRO a -> CliM a
doBackup f = gets cliBackupCfg >>= liftIO . runBackupRO f

data CliState = CliState
    { cliPath      :: FilePath
    , cliPathHash  :: Hash
    , cliPathEnts  :: [Ent]
    , cliBackupCfg :: BackupConfig
    , cliTerm      :: Terminal
    }

defaultCliState hash ents = CliState (decodeString "") hash ents

data ShellExec = ShellExec [String]
    deriving (Show,Eq)

--parseCli :: String -> ShellExec
--parseCli s = ShellExec []

printColor color sln = do
    t <- gets cliTerm
    let mf = getCapability t withForegroundColor
    liftIO $ runTermOutput t (maybe (termText sln) (\f -> f color $ termText sln) mf)

printColorLn color s = printColor color (s ++ "\n")

data LsOpt = LsAll | LsLong | LsHuman | LsRecurse
    deriving (Show,Eq)

withTime f = do
    t1 <- liftIO $ getCurrentTime
    a  <- f
    t2 <- liftIO $ getCurrentTime
    return (diffUTCTime t2 t1, a)

recurse delayDirs rootHash path fDir fFile = loop path
  where loop dir = do
            dirHash <- either error id <$> doBackup (resolvePath rootHash dir)
            ents    <- either error id <$> doBackup (readMeta dirHash)
            dirs    <- catMaybes <$> mapM (doEnt dir) ents
            mapM_ (\(fp, ent) -> fDir fp ent >> loop fp) dirs

        doEnt :: FilePath -> Ent -> CliM (Maybe (FilePath, Ent))
        doEnt dir ent = do
            let fp = dir </> entName ent
            case entType ent of
                EntDir  -> if delayDirs
                              then return $ Just (fp, ent)
                              else fDir fp ent >> loop fp >> return Nothing
                EntLink -> return Nothing
                _       -> fFile fp ent >> return Nothing

doLs subArgs = do
    let (optionArgs, nonOptions, errors) = getOpt Permute options subArgs
    case nonOptions of
        [] -> listOne optionArgs (decodeString "")
        l  -> mapM_ (listOne optionArgs . decodeString) l
  where listOne optArgs x = do
            currentTime <- liftIO $ getCurrentTime
            rootHash    <- gets cliPathHash
            d           <- gets cliPath
            let print = printEnt currentTime optArgs
            if LsRecurse `elem` optArgs
                then recurse True rootHash (d </> x) (\fp _ -> printColorLn White "" >> printColorLn Blue (encodeString fp) >> printColorLn White "") (\_ -> print)
                else do
                    dirHash     <- either error id <$> doBackup (resolvePath rootHash (d </> x))
                    ents        <- either error id <$> doBackup (readMeta dirHash)
                    forM_ ents print
        printEnt currentTime optArgs ent = do
                let isDot = head (encodeString (entName ent)) == '.'
                let color = case entType ent of
                                EntDir  -> Blue
                                EntLink -> Cyan
                                _       -> White
                unless (isDot && LsAll `notElem` optArgs) $ do
                    if LsLong `elem` optArgs
                        then do 
                                sizeField <- case entType ent of
                                                 EntFile -> do m <- doBackup (pathData (entHash ent)) >>= liftIO . getFileMetas
                                                               case m of
                                                                  Left _     -> return "missing"
                                                                  Right meta -> return $ showSZ $ fileMetaSize meta
                                                 _       -> return "     "
                                let name = encodeString $ entName ent
                                printColor White (showTime currentTime $ entMTime ent)
                                printColor White " "
                                printColor White (col 8 sizeField)
                                printColor White "  "
                                printColorLn color (name)
                        else printColorLn color (encodeString $ entName ent)
        showTime currentTime time = formatTime defaultTimeLocale "%F" (posixSecondsToUTCTime time)
        options =
            [ Option ['a'] ["all"] (NoArg LsAll) "show all entries"
            , Option ['l'] ["long"] (NoArg LsLong) "show in long format"
            , Option ['h'] ["human"] (NoArg LsHuman) "show unit with human helper"
            , Option ['R'] ["recurse"] (NoArg LsRecurse) "recurse in directories"
            ]
        col n s
            | length s < n = replicate (n - length s) ' ' ++ s
            | otherwise    = s

doCd :: FilePath -> CliM ()
doCd path = mapM_ loopCd (splitDirectories path)
  where loopCd :: FilePath -> CliM ()
        loopCd v | encodeString v == ".." = modify (\st -> st { cliPath = parent (cliPath st) })
                 | otherwise = do
            rootHash <- gets cliPathHash
            cwd      <- gets cliPath
            dirHash  <- either error id <$> doBackup (resolvePath rootHash cwd)
            ents     <- either error id <$> doBackup (readMeta dirHash)
            case find (\e -> entName e == v) ents of
                Nothing  -> liftIO $ putStrLn ("no such directory: " ++ show v)
                Just ent -> case entType ent of
                                EntDir -> modify (\st -> st { cliPath = cliPath st </> v })
                                _      -> liftIO $ putStrLn ("not a directory: " ++ show v)

data DuOpt = DuHuman
    deriving (Show,Eq)

doDu subArgs = do
    let (optionArgs, nonOptions, errors) = getOpt Permute options subArgs
    case nonOptions of
        [] -> gets cliPath >>= duOne optionArgs
        l  -> mapM_ (duOne optionArgs . decodeString) l
  where duOne optArgs x = do
            size <- liftIO $ newMVar 0
            rootHash <- gets cliPathHash
            cwd      <- gets cliPath
            (t, _)   <- withTime $ recurse False rootHash (cwd </> x) (\_ _ -> return ()) $ \_ ent -> do
                        p  <- doBackup $ pathData $ entHash ent
                        mm <- liftIO $ getFileMetas p
                        case mm of
                            Left _  -> return ()
                            Right m -> liftIO $ modifyMVar_ size (\v -> return $! (v + fileMetaSize m))
            sz <- liftIO $ readMVar size
            printColor White (showSZ sz)
            printColor White "   "
            printColorLn Blue  (encodeString x)
            
            {-
            cwd      <- gets cliPath
            rootHash <- gets cliPathHash
            dirHash  <- either error id <$> doBackup (resolvePath rootHash cwd)
            ents     <- either error id <$> doBackup (readMeta dirHash)
            -}
            return ()
        options =
            [ Option ['h'] ["human"] (NoArg DuHuman) "show unit with human helper"
            ]

data RmOpt = RmDetail
    deriving (Show,Eq)

doRm subArgs = do
    let (optionArgs, nonOptions, errors) = getOpt Permute options subArgs
    case nonOptions of
        [] -> return ()
        l  -> mapM_ (rm optionArgs . decodeString) l
  where rm optArgs x = do
            size <- liftIO $ newMVar 0
            rootHash <- gets cliPathHash
            cwd      <- gets cliPath
            (t, ()) <- withTime $ recurse False rootHash (cwd </> x) (\_ _ -> return ()) $ \fp ent -> do
                        p  <- doBackup $ pathData $ entHash ent
                        mm <- liftIO $ getFileMetas p
                        case mm of
                            Left _  -> return ()
                            Right m -> do
                                liftIO $ modifyMVar_ size (\v -> return $! (v + fileMetaSize m))
                                when (RmDetail `elem` optArgs) $ do
                                    printColor White ("freeing ")
                                    printColor Red (showSZ (fileMetaSize m))
                                    printColor White " from " 
                                    printColorLn Blue (encodeString fp)
                                doBackup $ removeData (entHash ent)
                                
            -- stats
            sz <- liftIO $ readMVar size
            printColor Red (showSZ sz)
            printColor White " freed in "
            printColorLn Blue  (show t)
        options =
            [ Option []    ["detail"] (NoArg RmDetail) "show every file removed"
            ]

runCli = do
    rootEnt <- gets (head . cliPathEnts)
    modify (\st -> st { cliPath = decodeString "/" </> entName rootEnt })
    forever $ do
        s <- catchCliM promptCommand (\exn -> putStrLn ("exception reading command " ++ show exn) >> return "")
        case properWords s of
            "ls":subArgs -> catchAll $ doLs subArgs
            "du":subArgs -> catchAll $ doDu subArgs
            "rm":subArgs -> catchAll $ doRm subArgs
            "cd":v:[]    -> catchAll $ doCd (decodeString v)
            "quit":_  -> liftIO $ exitSuccess
            "exit":_  -> liftIO $ exitSuccess
            []        -> return ()
            _         -> liftIO $ putStrLn ("unknown command: " ++ show s)
  where -- FIXME process quote properly
        properWords s = words s
        promptCommand :: CliM String
        promptCommand = do
            d <- gets cliPath
            let st = readlineStateDefault { rlPrompt = encodeString d ++ "> " }
            zipToList . rlCurrentLine <$> liftIO (readline st (\_ -> return Quit))
        catchAll f = catchCliM f (\exn -> putStrLn ("error: " ++ show exn))

main = do
    args <- getArgs
    case args of
        path:backupName:[] -> do
            bdir <- getBackupDir [ExplicitInstall path]
            let cfg = BackupConfig { backupDir = bdir, backupUseHardlinks = True }
            runCliM (backupName, cfg) runCli
        _ -> error "usage: shell <path> <backupname>"
