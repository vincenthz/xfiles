module Console.Display
    ( TerminalDisplay
    -- * Basic
    , displayInit
    , display
    , displayTextColor
    , displayLn
    -- * Progress Bar
    , ProgressBar
    , progress
    , progressTick
    -- * Summary line
    , Summary
    , summary
    , summarySet
    -- * Attributes
    , Color(..)
    , OutputElem(..)
    , termText
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Concurrent.MVar 
import           System.Console.Terminfo
import           System.IO

{-
data LineWidget =
      Text
    | Progress
    | Done
-}

data OutputElem =
      Bg Color
    | Fg Color
    | T  String
    | LeftT Int String
    | RightT Int String
    | NA
    deriving (Show,Eq)

data TerminalDisplay = TerminalDisplay (MVar Bool) Terminal

displayInit :: IO TerminalDisplay
displayInit = do
    hSetBuffering stdout NoBuffering
    cf <- newMVar False
    TerminalDisplay cf <$> setupTermFromEnv

display :: TerminalDisplay -> [OutputElem] -> IO ()
display tdisp@(TerminalDisplay clearFirst term) oelems = do
    cf <- modifyMVar clearFirst $ \cf -> return (False, cf)
    when cf $ runTermOutput term (maybe mempty id clearLineFirst)
    runTermOutput term $ renderOutput tdisp oelems
  where
        clearLineFirst = getCapability term clearEOL

renderOutput :: TerminalDisplay -> [OutputElem] -> TermOutput
renderOutput (TerminalDisplay _ term) to = mconcat $ map toTermOutput to
  where
        wF = maybe (const mempty) id $ getCapability term setForegroundColor
        wB = maybe (const mempty) id $ getCapability term setBackgroundColor
        rD = maybe mempty         id $ getCapability term restoreDefaultColors

        toTermOutput (Fg c) = wF c
        toTermOutput (Bg c) = wB c
        toTermOutput (T t)  = termText t
        toTermOutput (LeftT sz t)  = termText (t ++ replicate (sz - length t) ' ')
        toTermOutput (RightT sz t)  = termText (replicate (sz - length t) ' ' ++ t)
        toTermOutput NA     = rD

displayTextColor :: TerminalDisplay -> Color -> String -> IO ()
displayTextColor term color msg = do
    display term [Fg color, T msg]

displayLn :: TerminalDisplay -> Color -> String -> IO ()
displayLn disp color msg = displayTextColor disp color (msg ++ "\n")

data ProgressBar = ProgressBar TerminalDisplay ProgressBackend (MVar ProgressState)

type ProgressBackend = String -> IO ()

data Summary = Summary SummaryBackend
type SummaryBackend = [OutputElem] -> IO ()

data ProgressState = ProgressState
    { pgLhs     :: String
    , pgRhs     :: String
    , pgMax     :: Int
    , pgCurrent :: Int
    }

initProgressState :: Int -> ProgressState
initProgressState maxItems = ProgressState
    { pgLhs     = ""
    , pgRhs     = ""
    , pgMax     = maxItems
    , pgCurrent = 0
    }

progress :: TerminalDisplay
         -> Int
         -> (ProgressBar -> IO a)
         -> IO a
progress tdisp@(TerminalDisplay cf term) numberItems f = do
    let b = backend (getCapability term cursorDown)
                    (getCapability term carriageReturn)
                    (getCapability term clearEOL)
    pbar <- ProgressBar tdisp b <$> newMVar (initProgressState numberItems)

    progressStart pbar
    a <- f pbar
    displayLn tdisp White ""
    return a
  where
    backend :: Maybe (Int -> TermOutput)
            -> Maybe TermOutput
            -> Maybe TermOutput
            -> ProgressBackend
    backend _ (Just goHome) (Just clearEol) = \msg -> do
        runTermOutput term $ mconcat [clearEol, termText msg, goHome]
        modifyMVar_ cf $ return . const True
    backend _ _ _ = \msg ->
        displayLn tdisp White msg

showBar :: ProgressBar -> IO ()
showBar (ProgressBar _ backend pgsVar) = do
    pgs <- readMVar pgsVar
    let bar = getBar pgs
    backend bar
  where
    getBar (ProgressState lhs rhs maxItems current) =
            lhs `sep` bar `sep`
            (show current ++ "/" ++ show maxItems) `sep`
            rhs
      where
        sep s1 s2
            | null s1   = s2
            | null s2   = s1
            | otherwise = s1 ++ " " ++ s2

        bar
            | maxItems == current = "[" ++ replicate szMax fillingChar ++ "]"
            | otherwise           = "[" ++ replicate filled fillingChar ++ ">" ++ replicate (unfilled-1) ' ' ++ "]"

        fillingChar = '='

        unfilled, filled :: Int
        unfilled   = szMax - filled
        filled     = floor numberChar

        numberChar = fromIntegral szMax / currentProgress
        szMax      = 40

        currentProgress :: Double
        currentProgress = fromIntegral maxItems / fromIntegral current

progressStart :: ProgressBar -> IO ()
progressStart pbar = do
    showBar pbar
    return ()

progressTick :: ProgressBar -> IO ()
progressTick pbar@(ProgressBar _ _ st) = do
    modifyMVar_ st $ \pgs -> return $ pgs { pgCurrent = min (pgMax pgs) (pgCurrent pgs + 1) }
    showBar pbar
    return ()

summary :: TerminalDisplay -> IO Summary
summary tdisp@(TerminalDisplay cf term) = do
    let b = backend (getCapability term cursorDown)
                    (getCapability term carriageReturn)
                    (getCapability term clearEOL)
    return $ Summary b
  where
    backend :: Maybe (Int -> TermOutput)
            -> Maybe TermOutput
            -> Maybe TermOutput
            -> SummaryBackend
    backend _ (Just goHome) (Just clearEol) = \msg -> do
        runTermOutput term $ mconcat [clearEol, renderOutput tdisp msg, goHome]
        modifyMVar_ cf $ return . const True
    backend _ _ _ = \msg ->
        runTermOutput term $ mconcat [renderOutput tdisp msg]

summarySet :: Summary -> [OutputElem] -> IO ()
summarySet (Summary backend) output = do
    backend output
