{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tools.ChronoFs.Utils where

import           Basement.Bounded
import           Basement.Compat.IsList
import qualified Basement.String as S
import           Control.Applicative
import           Control.Monad (when, (>=>))
import           Control.Monad.IO.Class
import           Control.Monad.State (gets, MonadState)
import qualified Control.Exception as E
import           Console.Display
import           System.IO (withFile, IOMode(..), Handle)
import qualified System.Posix.Files.ByteString as RawFiles
import           Text.Printf
import           Tools.ChronoFs.Monad
import           Tools.ChronoFs.Config
import           Tools.ChronoFs.Types
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteArray.Encoding as BA
import           Prelude
import           System.Directory
import           System.FilePath

import           Crypto.Hash

data Color = Red | Green | Yellow
    deriving (Show,Eq)

colorToComponent :: Color -> ColorComponent
colorToComponent Red = zn64 1
colorToComponent Green = zn64 2
colorToComponent Yellow = zn64 3

printTerminalLn :: (MonadState BackupState m, MonadIO m) => Color -> [Char] -> m ()
printTerminalLn color s = printTerminal (colorToComponent color) (s ++ "\n")

printTerminal :: (MonadState BackupState m, MonadIO m) => ColorComponent -> String -> m ()
printTerminal color s = do
    term <- gets terminal
    liftIO $ displayTextColor term color (fromList s)

whenM :: Monad m => m Bool -> m () -> m ()
whenM cond f = cond >>= \b -> when b f

showSZ :: Integral a => a -> String
showSZ sz
    | sz < 1024               = printf "%d" (fromIntegral sz :: Integer)
    | sz < 1024 * 1024        = printf "%.3fK" (fromIntegral sz / 1024 :: Double)
    | sz < 1024 * 1024 * 1024 = printf "%.3fM" (fromIntegral sz / (1024 * 1024) :: Double)
    | otherwise               = printf "%.4fG" (fromIntegral sz / (1024 * 1024 * 1024) :: Double)

data AllOpt = UserInstall | ExplicitInstall String
    deriving (Show,Eq)

getBackupDir :: [AllOpt] -> IO FilePath
getBackupDir opts = do
    home <- getHomeDirectory
    let userPath = (home </> backupDirName)
    return $ maybe userPath id $ foldl (doAcc userPath) Nothing opts
  where doAcc _  _   (ExplicitInstall p) = Just p
        doAcc hf _    UserInstall        = Just hf
        --doAcc _  acc _                   = acc

runIO :: IO a -> Backup b -> (a -> Backup b) -> Backup b
runIO io errCont cont = do
    r <- liftIO ((Right <$> io) `E.catch` exnLog)
    case r of
        Left err -> incErrors >> reportErr err >> errCont
        Right a  -> cont a
    where reportErr = printTerminalLn Red
          exnLog :: E.SomeException -> IO (Either String a)
          exnLog e = return $ Left (show e)

runIO_ :: IO () -> Backup ()
runIO_ io = runIO io (return ()) return

hexHashAsBs :: Hash -> BC.ByteString
hexHashAsBs (Hash b) = BA.convertToBase BA.Base16 b

hexHash :: Hash -> [Char]
hexHash = BC.unpack . hexHashAsBs

hashHexAsBs :: B.ByteString -> Either String Hash
hashHexAsBs =
    convertBase16 >=> maybe (Left "cannot convert hash") (Right . Hash) . digestFromByteString
  where
    convertBase16 :: BC.ByteString -> Either String BC.ByteString
    convertBase16 = BA.convertFromBase BA.Base16

hashHex :: String -> Either String Hash
hashHex = hashHexAsBs . BC.pack

getFileMetas :: FilePath -> IO (Either String FileMeta)
getFileMetas filepath = catchIO ("getFileMetas " ++ show filepath) $ do
    fs <- RawFiles.getSymbolicLinkStatus (UTF8.fromString filepath)
    return $ FileMeta (toFt fs)
                      (RawFiles.fileMode fs)
                      (RawFiles.modificationTimeHiRes fs)
                      (RawFiles.statusChangeTimeHiRes fs)
                      (fromIntegral $ RawFiles.fileSize fs)
    where toFt fs
            | RawFiles.isBlockDevice fs     = BlockDevice
            | RawFiles.isCharacterDevice fs = CharDevice
            | RawFiles.isNamedPipe fs       = NamedPipe
            | RawFiles.isRegularFile fs     = RegularFile
            | RawFiles.isDirectory fs       = Directory
            | RawFiles.isSymbolicLink fs    = SymbolicLink
            | RawFiles.isSocket fs          = Socket
            | otherwise                     = error "unrecognized file type"

getFileHash :: FilePath -> IO Hash
getFileHash f = withFile f ReadMode $ \h -> loop h hashInit
  where loop :: Handle -> Context HashT -> IO Hash
        loop h !c = do
            r <- B.hGet h (16*1024)
            if B.length r == 0
                then return $! Hash $! hashFinalize c
                else loop h (hashUpdate c r)

catchIO :: String -> IO a -> IO (Either String a)
catchIO s f = (Right <$> f) `E.catch` (\(exn :: E.IOException) -> return $ Left (s ++ " : " ++ show exn))
