{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Main where

import           Basement.Compat.IsList
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

pattern White :: ColorComponent
pattern White = 0

pattern Red :: ColorComponent
pattern Red = 1

pattern Green :: ColorComponent
pattern Green = 2

pattern Blue :: ColorComponent
pattern Blue = 3

pattern Cyan :: ColorComponent
pattern Cyan = 4

pattern Yellow :: ColorComponent
pattern Yellow = 5


run h ctx = case configUnique ctx of
    Nothing -> do
        putStrLn "no unique configuration"
        exitFailure
    Just unique -> do
        let srcs = configUniqueSource unique
            dst = configUniqueDestination unique
            ignores = configUniqueIgnore unique

        let hasher = hasherInit h
            hashfsConf = makeConf [2] hasher OutputBase32 dst

        dstExist <- doesDirectoryExist dst
        when (not dstExist) $ Local.initialize hashfsConf

        let initSt = (0, 0, 0, 0) :: (Int, Int, Int, FileSize)

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

                        let color = if newLink then Yellow else if existsAlready then Green else Red

                        -- dig <- liftIO $ hashFile hasher file
                        liftIO $ display term [Fg color, LeftT 26 (fromList $ show dig), T " ", Fg Green, T (fromList file) ]
                        liftIO $ displayLn term White ""
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
                | elem dir ignores       = return False
                | otherwise              = do
                    liftIO $ displayLn term Blue (fromList dir)
                    return True
        displayLn term Red "unique"
        (files, skipped, saved, savedSz) <- execStateT (mapM_ (\src -> dirTraverse_ src fileCallback dirCallback) srcs) initSt

        displayLn term Red $ fromList ("files      : " ++ show files)
        displayLn term Red $ fromList ("skipped    : " ++ show skipped)
        displayLn term Red $ fromList ("# saved    : " ++ show saved)
        displayLn term Red $ fromList ("saved size : " ++ show savedSz ++ " bytes")
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
