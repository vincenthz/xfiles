module Main where

import           System.Directory.Traverse
import           Console.Options
import           Console.Display
import           Data.Maybe
import           Data.Monoid
import           Data.List (isSuffixOf)
import           System.Environment
import           System.Exit
import           System.Directory
import           Control.Monad.Trans.State
import           Control.Monad.IO.Class
import           Control.Monad

import           Storage.HashFS
import qualified Storage.HashFS.Local as Local
import           Storage.HashFS.Client
import           Storage.HashFS.Protocol
import           Storage.HashFS.IO
import           Storage.HashFS.Path
import           Storage.HashFS.ConfigFile
import           Storage.HashFS.Meta
import           Storage.HashFS.Query
import           Storage.HashFS.Utils
import           Storage.HashFS.Hasher

import           Storage.Utils

import           System.Posix.Files (createLink)

run h ctx = case configUnique ctx of
    Nothing -> do
        putStrLn "no unique configuration"
        exitFailure
    Just unique -> do
        let src = configUniqueSource unique
            dst = configUniqueDestination unique

        let hasher = hasherInit h
            hashfsConf = makeConf [2] hasher OutputBase32 dst

        dstExist <- doesDirectoryExist dst
        when (not dstExist) $ Local.initialize hashfsConf

        let initSt = (0, 0, 0, 0)

        term <- displayInit

        let fileCallback file = do
                r <- liftIO $ getFileInfoPlus file
                case r of
                    Nothing                      -> return ()
                    Just (fsz, mtime, linkcount) -> do
                        -- importFile :: HashAlgorithm h => HashFSConf h -> ImportType -> FilePath -> IO (Bool, Digest h)
                        (existsAlready, dig) <- liftIO $ Local.importFile hashfsConf Local.ImportHardlink file

                        newLink <- if existsAlready && linkcount == 1
                            then do
                                let destPath = getPath hashfsConf dig
                                liftIO $ do
                                    removeFile file
                                    createLink destPath file
                                return True
                            else
                                return False

                        let color = if existsAlready then Red else Green

                        -- dig <- liftIO $ hashFile hasher file
                        liftIO $ display term [Fg color, LeftT 26 (show dig), T " ", Fg Green, T (file) ]
                        liftIO $ displayLn term Red ""
                        modify $ \(files,skipped,saved,savedSz) ->
                            ( files+1
                            , skipped + if existsAlready then 1 else 0
                            , saved + if newLink then 1 else 0
                            , savedSz + if newLink then fsz else 0
                            )
                        return ()
            dirCallback dir
                | dir == dst             = return False
                | isSuffixOf "/.git" dir = return False
                | otherwise              = do
                    liftIO $ displayLn term Blue dir
                    return True
        displayLn term Red "unique"
        (files, skipped, saved, savedSz) <- execStateT (dirTraverse_ src fileCallback dirCallback) initSt

        displayLn term Red ("files      : " ++ show files)
        displayLn term Red ("skipped    : " ++ show skipped)
        displayLn term Red ("# saved    : " ++ show saved)
        displayLn term Red ("saved size : " ++ show savedSz ++ " bytes")
        exitSuccess

main = defaultMain $ do
    programName "hashfs-unique"
    programDescription "move all the file to a unique file name based on their hash"

    command "run" $ do
        description "run the unique algorithm on files"
        source <- flagParam (FlagShort 's' <> FlagLong "source" <> FlagDescription "source path to scan")
                            (FlagRequired Right)
        destination <- flagParam (FlagShort 'd' <> FlagLong "destination" <> FlagDescription "destination path for unique file")
                                 (FlagRequired Right)
        --algorithm <- flag (FlagShort ''
        action $ \toParam -> withConfigAt ".hashfs-unique" $ run -- (toParam)
