module Tools.ChronoFs.Display where

--import System.Console.Terminfo
import Tools.ChronoFs.Monad
import Tools.ChronoFs.Types
import Tools.ChronoFs.Utils
import Control.Monad.State

showStat :: Bool -> Backup ()
showStat nl = do
    (Stats e d s p sz) <- gets stats
    printTerminalLn Yellow (show p ++ " done -- " ++ showSZ sz ++ " added (" ++ show s ++ "S," ++ show d ++ "D," ++ show e ++ "E)" ++ (if nl then "\n" else "\r") )
