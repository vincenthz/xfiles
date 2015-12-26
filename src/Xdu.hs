{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Console.Display
import           Console.Options
import           Data.Char
import           Data.Maybe (catMaybes)
import qualified Data.Map as M
import           Data.Monoid
import           System.Directory.Traverse
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.Directory
import           Tools.ChronoFs (showSZ)
import           Tools.Utils

import qualified Paths_xfiles

data Format = Image | ExeLibObj | Music | Video | Text | Document | Other
    deriving (Show,Eq,Bounded,Enum)

formatExtensions = M.fromList $ concat
    [ [ (x, Video)    | x <- ["avi","mov","mkv","mp4"] ]
    , [ (x, Music)    | x <- ["mp3","ogg"] ]
    , [ (x, Image)    | x <- ["jpeg", "jpg", "tiff", "png", "bmp"] ]
    , [ (x, Text)     | x <- ["txt", "cfg"] ]
    , [ (x, Document) | x <- ["doc", "pdf", "ps", "xls", "csv"] ]
    , [ (x, ExeLibObj) | x <- ["o", "dynlib", "a", "dll", "so", "rlib"] ]
    ]

getFormat = maybe Other id . flip M.lookup formatExtensions

filePathToFormat :: FilePath -> Format
filePathToFormat = getFormat . map toLower . dropDot . takeExtension
  where dropDot ('.':l) = l
        dropDot l       = l

data St = St
    { stDirs  :: !Int
    , stFiles :: !Int
    , stCurrentDir :: !String
    , stData  :: !Bytes
    , stFormats :: FormatSize
    }

instance Monoid St where
    mempty = newSt
    mappend (St d1 f1 _ sz1 ft1) (St d2 f2 _ sz2 ft2) =
        St (d1+d2) (f1+f2) "" (sz1 `mappend` sz2) (ft1 `mappend` ft2)

newtype FormatSize = FormatSize (Bytes, Bytes, Bytes, Bytes, Bytes, Bytes, Bytes)
    deriving (Show,Eq)

instance Monoid FormatSize where
    mempty = FormatSize (mempty,mempty,mempty,mempty,mempty,mempty,mempty)
    mappend (FormatSize (a1,b1,c1,d1,e1,f1,g1))
            (FormatSize (a2,b2,c2,d2,e2,f2,g2)) =
        FormatSize (a1`mappend`a2,b1`mappend`b2,c1`mappend`c2,d1`mappend`d2,e1`mappend`e2,f1`mappend`f2, g1`mappend`g2)

toFormatSize :: Format -> Bytes -> FormatSize
toFormatSize Image sz     = FormatSize (sz,mempty,mempty,mempty,mempty,mempty,mempty)
toFormatSize Video sz     = FormatSize (mempty,sz,mempty,mempty,mempty,mempty,mempty)
toFormatSize Music sz     = FormatSize (mempty,mempty,sz,mempty,mempty,mempty,mempty)
toFormatSize Text sz      = FormatSize (mempty,mempty,mempty,sz,mempty,mempty,mempty)
toFormatSize Document sz  = FormatSize (mempty,mempty,mempty,mempty,sz,mempty,mempty)
toFormatSize ExeLibObj sz = FormatSize (mempty,mempty,mempty,mempty,mempty,sz,mempty)
toFormatSize Other sz     = FormatSize (mempty,mempty,mempty,mempty,mempty,mempty,sz)

onlyOneFormat :: FormatSize -> Bool
onlyOneFormat (FormatSize (a,b,c,d,e,f,g)) = length (filter (/= mempty) [a,b,c,d,e,f,g]) <= 1

newSt :: St
newSt = St
    { stDirs       = 0
    , stFiles      = 0
    , stCurrentDir = ""
    , stData       = mempty
    , stFormats    = mempty
    }

incDirs mv = modifyMVar_ mv $ \st -> return $ st { stDirs = stDirs st + 1 }
incFiles mv = modifyMVar_ mv $ \st -> return $ st { stFiles = stFiles st + 1 }
appendSize mv sz = modifyMVar_ mv $ \st -> return $ st { stData = mappend (stData st) sz }
setCurrentDir mv s = modifyMVar_ mv $ \st -> return $ st { stCurrentDir = s }

appendFileStat mv sz fmt = modifyMVar_ mv $ \st -> return $
    st { stFiles   = stFiles st + 1
       , stData    = stData st `mappend` sz
       , stFormats = toFormatSize fmt sz `mappend` stFormats st
       }

printStats detailed summ mv = readMVar mv >>= \st -> do
    either display summarySet summ $ (txt st)
  where
    txt st = [Fg Red, LeftT 6 (show $ stFiles st), T " "
             ,Fg Blue, RightT 8 (show $ stData st), T " "
             ]
             ++ [Fg Green, T (stCurrentDir st), T " "]
             ++ formatDisplay (showFormat $ stFormats st)
             ++ [NA]
      where
        formatDisplay :: [a] -> [a]
        formatDisplay = either (const id) (const (const [])) summ
                      . if not detailed || onlyOneFormat (stFormats st) then const [] else id

        showFormat :: FormatSize -> [OutputElem]
        showFormat (FormatSize (a,b,c,d,e,f,g)) =
            (concatMap render $ zip "IVMTDEO" [a,b,c,d,e,f,g])
          where render (cFormat, sz)
                    | sz == mempty = []
                    | otherwise    = [Fg Red, T [cFormat,':'], Fg Cyan, RightT 4 (show $ BytesCondensed sz), T " "]

while a f = f a >>= \(b, a') -> if b then while a' f else return ()

xdu term detailed dirs = do
    display term [Fg Red, LeftT 6 "#file ", T " ", RightT 8 "size", NA]
    displayLn term White ""
    sts <- mapM showStats dirs

    displayLn term Blue "========================================================="
    printStats detailed (Left term) =<< newMVar (mconcat $ catMaybes sts)

    displayLn term White ""
    displayLn term White ""
  where
    showStats :: FilePath -> IO (Maybe St)
    showStats path = do
        isDir  <- doesDirectoryExist path
        isFile <- doesFileExist path
        if | isDir     -> showDirStats path
           | isFile    -> showFileStats path
           | otherwise -> showError path

    showFileStats path = do
        st <- newMVar mempty
        sz <- getFileSize path
        appendFileStat st sz (filePathToFormat path)
        setCurrentDir st path
        printStats detailed (Left term) st
        displayLn term Red ""
        Just <$> readMVar st

    showError path = do
        displayLn term Red ("error: " ++ path ++ " : doesn't exist")
        return Nothing

    showDirStats :: FilePath -> IO (Maybe St)
    showDirStats rootDir = do
        st   <- newMVar mempty
        summ <- summary term
        setCurrentDir st rootDir
        let dirCb dir = do
                let x     = depthDiff rootDir dir
                    depth = length x
                when (depth < depthMax) $ setCurrentDir st (rootDir ++ " ... " ++ concat x)
                incDirs st
                return True
            fileCb f = do
                --displayLn term Red (f ++ " : " ++ show (takeExtension f))
                sz <- getFileSize f
                appendFileStat st sz (filePathToFormat f)
                return ()

        -- create a thread for the traversal while printing every XXX ms
        finished <- newEmptyMVar
        _ <- forkIO $ do
            dirTraverse_ rootDir fileCb dirCb
            putMVar finished ()
        while 20000 $ \delay -> do
            isFinished <- not <$> isEmptyMVar finished
            if isFinished
                then return (False, delay)
                else do
                    printStats detailed (Right summ) st
                    threadDelay delay
                    return (True, min (delay + 20000) 200000)
        setCurrentDir st rootDir
        printStats detailed (Left term) st
        displayLn term Red ""
        Just <$> readMVar st

    depthMax = 4

    depthDiff rDir aDir = diff (splitPath rDir) (splitPath aDir)
      where diff (a:as) (b:bs)
                | a == b    = diff as bs
                | otherwise = diff as bs -- weird case. not sure what to do.
            diff l []  = l
            diff [] l  = l

-- extensive disk usage
main = do
    term <- displayInit
    defaultMain $ do
        programName "xdu"
        programDescription "interactive and detailed disk usage reporting"
        programVersion (Paths_xfiles.version)
        detailedFlag <- flag $ FlagShort 'd' <> FlagLong "detailed"
        allArgs      <- remainingArguments "FILE"
        action $ \flags args ->
            xdu term
                (maybe False id $ flags detailedFlag)
                (if null (args allArgs) then ["."] else args allArgs)

