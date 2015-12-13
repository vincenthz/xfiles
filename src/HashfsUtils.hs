module Main where


import qualified Crypto.PubKey.Ed25519 as Ed25519
import           Crypto.Random
import           Hashfs.Common
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteArray.Encoding as B
import           System.Environment

toPublic args =
    case args of
        []     -> error "usage: hashfs-utils to-public <file>"
        file:_ -> do
            (_,f) <- readKeyFile file
            putStrLn $ BC.unpack (B.convertToBase B.Base16 f)
   
cmds =
    [ ("to-public", toPublic)
    ]

err msg = do
    putStrLn ("error: " ++ msg ++ ", known commands:")
    mapM_ (putStrLn . (++ "  ") . fst) cmds

main = do
    args <- getArgs
    case args of
        []   -> err "no command"
        c:cs ->
            case lookup c cmds of
                Nothing -> err ("unknown command: " ++ c)
                Just f  -> f cs 
