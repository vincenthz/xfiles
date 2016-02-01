module Main where

import           Network.Socket hiding (send,recv)
import           System.Timeout
import           System.Environment
import           System.FilePath
import           System.Directory
import           System.Exit
import           Control.Monad
import           Control.Concurrent
import           Text.Read
import           Data.Maybe
import           Data.List (find)
import           Data.Monoid
import qualified Crypto.PubKey.Ed25519 as Ed25519
import           Crypto.Error
--import qualified Tools.Config as Config

import qualified Data.ByteArray.Encoding as B
import qualified Data.ByteArray as B
import qualified Data.ByteString as B (readFile)
import qualified Data.ByteString.Char8 as BC

--import           Network.LSP
import           Storage.HashFS
import           Storage.HashFS.Client
import           Storage.HashFS.Protocol
import           Storage.HashFS.IO
import           Storage.HashFS.Utils
import           Storage.Utils

import           Hashfs.Common

import           Console.Options
import           Console.Display

serverAddr :: String
serverAddr = "127.0.0.1"

serverPort :: Int
serverPort = 2902

main :: IO ()
main = defaultMain $ do
    programName "hashfs-cli"
    programDescription "hashfs -- command line interface"

    cfgFlag <- flagParam (FlagLong "config" <> FlagShort 'c'
                         <> FlagDescription "client configuration to use (default \"client.priv\")")
                         (FlagRequired Right)
                   -- <> FlagDefault "client.priv"
                   -- <> FlagArg ""

    command "replicate" $ do
        serverFlag <- flag $ FlagLong "server"
        serverKey  <- flag $ FlagLong "server-key"
        action $ \_ -> do
            key <- readKeyFile "client.priv"
            --a <- getArgs
            -- hardcoded server public key in hex
            let a = ["fb05f8e67af98044cee008712ff175da695b2b18ed65c1ca47f5f07e73ddfed9"]
            let servKey = parsePublicKeyHex (a !! 0)

            let conCfg = ConnectionConfig
                    { concfgHostname    = serverAddr
                    , concfgPort        = serverPort
                    , concfgExpectedKey = servKey
                    , concfgMyKey       = key
                    }
            client <- clientNew conCfg
            clientEstablish client
            putStrLn ("connected to server on port " ++ show serverPort)


            -- FIXME do something

            clientShut client
            return ()

    command "config" $ do
        description "print the config read from the environment and quit"
        action $ \_ -> do
            withConfig $ \context -> do
                mapM_ (putStrLn . show) (contextProviders context)
    command "import" $ do
        description "import individual or group of files"
        repoD <- flagParam (   FlagLong "repository"
                           <> FlagShort 'r'
                           <> FlagDescription "explicitly specify the repository used for import"
                           ) (FlagRequired Right)
        groupD <- flagMany $ flagParam (FlagLong "group" <> FlagShort 'g' <> FlagDescription "tag with a group") (FlagRequired Right)
        tagD   <- flagMany $ flagParam (FlagLong "tag" <> FlagShort 't' <> FlagDescription "add a tag") (FlagRequired Right)
        personD <- flagMany $ flagParam (FlagLong "person" <> FlagShort 'p' <> FlagDescription "tag with a person") (FlagRequired Right)

        argDescs <- remainingArguments "arguments"
        action $ \toParam -> do
            disp <- displayInit
            let args = toParam argDescs
                repo = toParam repoD
            let metaTags = map (Tag (Just Group)) (toParam groupD)
                        ++ map (Tag (Just Person)) (toParam personD)
                        ++ map (Tag (Just Other)) (toParam tagD)
            withConfig (doImport disp metaTags repo args)

    command "query" $ do
        description "query meta database"

doImport disp metaTags repo args context = do
    let provs = contextProviders context

    forM_ args $ \filePath -> do
        let ext = takeExtension filePath
        case repo of
            Nothing -> do
                putStrLn "automatic import routing not implemented"
                exitFailure
            Just repoName -> do
                let prov = findProvider provs repoName

                di <- grabDataInfo (contextMetaviders context) filePath

                (dup, d) <- importInto (fmap (\(a,b) -> (a,b,metaTags)) di) prov ImportCopy filePath
                display disp [Fg Green, T (if dup then "" else "✓ "), NA, T $ show d, T " ", T filePath, T "\n"]

--grabDataInfo :: [MetaVider] -> FilePath -> IO (Maybe (MetaVider, DataInfo))
grabDataInfo []    _  = return Nothing
grabDataInfo (m:_) fp = do
    cwd <- getCurrentDirectory
    let fullpath = if isAbsolute fp then fp else cwd </> fp
    let (dirName, fileName) = splitFileName fullpath
    di <- catchIO (getFileInfo fp)
    case di of
        Nothing            -> return Nothing
        Just (fsz,modTime) -> return $ Just (m, DataInfo
            { dataSize     = fsz -- in bytes
            , dataDate     = Just modTime
            , dataDirName  = Just dirName
            , dataFileName = Just fileName
            })

findProvider :: [Provider h] -> String -> Provider h
findProvider provs name =
    maybe (error $ "cannot find repository for " ++ name) id $ find (\p -> providerName p == name) provs
