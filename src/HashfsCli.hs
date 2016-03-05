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
import           Data.List (find, intercalate)
import           Data.Monoid
import qualified Crypto.PubKey.Ed25519 as Ed25519
import           Crypto.Error
import           Crypto.Hash (Digest)
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
import           Storage.HashFS.Meta
import           Storage.HashFS.Query
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

    command "untagged" $ do
        description "query meta database about untagged digest"
        asPath   <- flag (FlagShort 'p' <> FlagLong "show-path" <> FlagDescription "show path instead of digest")
        asBase   <- flag (FlagShort 'b' <> FlagLong "base32" <> FlagDescription "show digest in base32 instead of hexadecimal")
        action $ \toParam -> withConfig $ \context -> do
            let showDigest d
                    | toParam asPath    = do
                        mp <- getLocalPath (contextProviders context) d
                        case mp of
                            Nothing -> putStrLn (show d ++ " not available locally")
                            Just p  -> putStrLn p
                    | otherwise         = do
                        let dg = if toParam asBase then showBase32 d else show d
                        putStrLn dg
            let metaviders = contextMetaviders context
            digests <- metaFindDigestsNotTagged (head metaviders)
            when (null digests) $ error "no untagged digest found"
            mapM_ showDigest digests

    command "query" $ do
        description "query meta database"
        queryArg <- argument "query" Right
        showTags <- flag (FlagShort 't' <> FlagLong "show-tags" <> FlagDescription "show all the tags for each digest matching the query")
        asPath   <- flag (FlagShort 'p' <> FlagLong "show-path" <> FlagDescription "show path instead of digest")
        asBase   <- flag (FlagShort 'b' <> FlagLong "base32" <> FlagDescription "show digest in base32 instead of hexadecimal")
        action $ \toParam -> withConfig $ \context -> do
            let query = toParam queryArg
            --putStrLn $ show query
            let showDigest withTags d
                    | toParam asPath    = do
                        mp <- getLocalPath (contextProviders context) d
                        case mp of
                            Nothing -> putStrLn (show d ++ " not available locally")
                            Just p  -> putStrLn p
                    | otherwise         = do
                        t <- if withTags
                                then intercalate " " . map show <$> metaDigestGetTags (head $ contextMetaviders context) d
                                else return ""
                        let dg = if toParam asBase then showBase32 d else show d
                        putStrLn $ dg ++ " " ++ t
            case parseQuery query of
                Left err  -> putStrLn ("query error: " ++ err)
                Right dq  -> do
                    let metaviders = contextMetaviders context
                    digests <- metaFindDigestsWhich (head metaviders) dq
                    when (null digests) $ error "no digest found"
                    mapM_ (showDigest (toParam showTags)) digests
    command "meta" $ do
        description "query / change meta database"
        command "rename" $ do
            command "tag" $ do
                t1 <- argument "old" (Right . tagFromString)
                t2 <- argument "new" (Right . tagFromString)
                action $ \toParam -> withConfig $ \context -> do
                    let metaviders = contextMetaviders context
                    r <- metaRenameTag (head metaviders) (toParam t1) (toParam t2)
                    metaCommit (head metaviders)
                    putStrLn $ "success: " ++ show r
        command "list" $ do
            command "tag" $ do
                action $ \toParam -> withConfig $ \context -> do
                    let metaviders = contextMetaviders context
                    tags <- metaGetTags (head metaviders)
                    mapM_ (putStrLn . tagToString) tags

doImport disp metaTags repo args context = do
    forM_ args $ \filePath -> do
        let ext = takeExtension filePath
        case repo of
            Nothing -> do
                putStrLn "automatic import routing not implemented"
                exitFailure
            Just repoName -> do
                let prov = findProvider context repoName

                di <- grabDataInfo (contextMetaviders context) filePath

                (dup, d) <- importInto (fmap (\(a,b) -> (a,b,metaTags)) di) prov ImportCopy filePath
                display disp [Fg Green, T (if dup then "" else "✓ "), NA, T $ show d, T " ", T filePath, T "\n"]

showBase32 :: Digest h -> String
showBase32 = show

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

findProvider :: Context h -> String -> Provider h
findProvider provs name =
    maybe (error $ "cannot find repository for " ++ name) id $ findProviderByName provs name
