module Main where

import           Network.Socket hiding (send,recv)
import           System.Timeout
import           Control.Monad
import           Control.Concurrent
import           Text.Read
import           Data.Maybe
import qualified Crypto.PubKey.Ed25519 as Ed25519
import           Crypto.Error
import qualified Tools.Config as Config

import qualified Data.ByteArray.Encoding as B
import qualified Data.ByteArray as B
import qualified Data.ByteString as B (readFile)
import qualified Data.ByteString.Char8 as BC

import           Network.LSP
import           Storage.HashFS.Server
import           Storage.HashFS.Protocol
import           Storage.HashFS.IO
import           Storage.Utils
import           Hashfs.Common

{-
processClient lsp = do
    (verb, payloadLength) <- waitCommand lsp
    case verb of
        Enumerate prefix ->
            -- run (makeConfSHA256 [2,2])
            putStrLn "not implemented"
        Get e            ->
            putStrLn "not implemented"
        Put e            ->
            putStrLn "not implemented"
        Exist e          ->
            putStrLn "not implemented"
        Create           -> 
            putStrLn "not implemented"
-- enumerate :: String -> IO [Hash]
-- put :: Hash -> Data -> IO ()
-- get :: Hash -> IO Data
-- create :: Data -> IO Hash
-- exists :: Hash -> IO Bool
    return ()
    
readPrivKey :: String -> Either String (Ed25519.SecretKey, Ed25519.PublicKey)
readPrivKey s =
    case B.convertFromBase B.Base16 $ BC.pack s of
        Left err -> Left ("converting " ++ s ++ " into key")
        Right b  ->
            case Ed25519.secretKey (b :: BC.ByteString) of
                CryptoFailed err -> Left ("converting " ++ s ++ "into key: " ++ show err)
                CryptoPassed r   -> Right (r, Ed25519.toPublic r)
-}

readKey s =
    B.convertFromBase B.Base16 $ BC.pack s

{- API idea:
    parseConfig "hashfds.conf" $ do
        r <- section "listen" $ do
            port <- key "port" "port to use" $ parseIntBound (\i -> i > 0 && i < 65536)
            k    <- key "key" "private key" undefined
            as   <- manyKey "allowed" "public key in base16 allowed to communicate with this instance" undefined
            return (port, k, as)
        return r
-}

data MainConfig = MainConfig
    { cfgPort       :: Int
    , cfgPrivateKey :: (NodeSecretKey, NodePublicKey)
    , cfgUsers      :: [User]
    , cfgDbs        :: [Db]
    }

data User = User
    { userName   :: String
    , userPubKey :: NodePublicKey
    }

data Db = DbLocal FilePath
    deriving (Show,Eq)

-- type Context = String
-- class Failable f where
--      tryTo :: Context -> f a -> b

getConfig = do
    cfg <- Config.readConfigPath "hashfsd.conf"
    let specifiedPort  = maybe 2902 id $ (Config.get [cfg] "listen" "port" >>= readMaybe)
        privateKeyFile = maybe (error "no key specified in listen section") id $ Config.get [cfg] "listen" "key"

    k <- readKeyFile privateKeyFile

    let parseUser [n,p] =
            let pubkey = parsePublicKeyHex p
             in Just $ User n pubkey
        parseUser _ =
            Nothing

    let userSections = Config.getAllSections [cfg] "user"
        users        = flip map userSections $ \sect ->
                        case mapM (Config.kvsGet sect) ["name", "pubkey"] of
                            Nothing -> Nothing
                            Just l  -> parseUser l

    let parseDb [ty,pa] =
            case ty of
                "local" -> Just $ DbLocal pa
                _       -> Nothing
        parseDb _ = Nothing

    let dbSections = Config.getAllSections [cfg] "db"
        dbs        = flip map dbSections $ \sect ->
                        case mapM (Config.kvsGet sect) ["type", "path"] of
                            Nothing -> Nothing
                            Just l  -> parseDb l
        
    return $ MainConfig { cfgPort       = specifiedPort
                        , cfgPrivateKey = k
                        , cfgUsers      = catMaybes users -- FIXME somewhat silent dropping of invalid users. error on pubkey invalid. FIX
                        , cfgDbs        = catMaybes dbs   -- FIXME ..
                        }

doGet :: EntityId -> IO (Either ProtocolError (FileSize, DataReader))
doGet eid = do
    putStrLn ("getting entity " ++ show eid)
    return $ Left Unsupported

doPut :: EntityId -> FileSize -> IO (Either ProtocolError DataWriter)
doPut eid fz = do
    putStrLn ("putting entity " ++ show eid ++ " filesize=" ++ show fz)
    return $ Left Unsupported

main = do
    cfg <- getConfig

    -- create listening sockets
    addr <- inet_addr "127.0.0.1"
    sock <- socket AF_INET Stream 0
    let sockaddr = SockAddrInet (fromIntegral $ cfgPort cfg) addr
    bind sock sockaddr
    listen sock 10

    putStrLn ("listening on " ++ show (cfgPort cfg))

    let allowedKeys = map userPubKey $ cfgUsers cfg
        keyByAddr _ = (cfgPrivateKey cfg, allowedKeys)

    let impl = ServerImplStore
                { serverImplGet = doGet
                , serverImplPut = doPut
                }

    serv <- serverNew impl keyByAddr sock
    serverWait serv
