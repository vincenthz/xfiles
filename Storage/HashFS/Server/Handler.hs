{-# LANGUAGE ScopedTypeVariables #-}
module Storage.HashFS.Server.Handler
    ( FireWall(..)
    , FireWallDenied(..)
    , FireWallStatus(..)
    , Handler
    , Listener
    , listenerCreate
    , handlerCreate
    , handlerAddListener
    -- * no export from here
    , grab
    ) where

import           Basement.Compat.Semigroup
import           Control.Concurrent (ThreadId, forkIO)
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Exception hiding (Handler)
import           Network.Socket hiding (send, recv)
import qualified Data.Map as M
import           Data.IORef

type TableElem = (ThreadId, Socket, SockAddr, IO ())

data Table = Table
    { byThread :: M.Map ThreadId TableElem
    , byAddr   :: M.Map SockAddr [TableElem]
    , numConns :: !Int
    }

data FireWall = FireWall
    { blacklist :: SockAddr -> Bool
    , whitelist :: SockAddr -> Bool
    }

instance Semigroup FireWall where
    (<>) f1 f2 =
        FireWall { blacklist = \a -> blacklist f1 a || blacklist f2 a
                 , whitelist = \a -> whitelist f1 a && whitelist f2 a
                 }

instance Monoid FireWall where
    mempty        = FireWall (\_ -> False) (\_ -> True)
    mappend f1 f2 =
        FireWall { blacklist = \a -> blacklist f1 a || blacklist f2 a
                 , whitelist = \a -> whitelist f1 a && whitelist f2 a
                 }

data FireWallDenied = BlackListed | TooManyClients
    deriving (Show,Eq)

data FireWallStatus =
      FireWallAuthorized
    | FireWallDenied FireWallDenied
    deriving (Show,Eq)

data Listener a = Listener
    { listeningSocket :: Socket
    , firewall        :: FireWall
    , onConnect       :: Socket -> SockAddr -> IO (Either String (a, IO ()))
    }

data Handler a = Handler
    { listeners :: MVar [Listener a]
    , table     :: MVar Table
    , runner    :: a -> IO ()
    }

grab :: MVar b -> (b -> IO a) -> IO a
grab mvar f = modifyMVar mvar (\b -> f b >>= \a -> return (b, a))

newTable :: Table
newTable = Table M.empty M.empty 0

insertTable :: TableElem -> Table -> Table
insertTable tableElem@(tId, _, addr, _) t =
    Table { byThread = M.insert tId tableElem (byThread t) 
          , byAddr   = M.alter (mapAppend tableElem) addr (byAddr t)
          , numConns = numConns t + 1
          }
  where
    mapAppend x Nothing = Just [x]
    mapAppend x (Just l) = Just (x:l)

removeTableTid :: ThreadId -> Table -> Table
removeTableTid tid t =
    case M.lookup tid (byThread t) of
        Nothing                 -> t
        Just (_, sock, addr, _) ->
            Table { byThread = M.delete tid (byThread t)
                  , byAddr   = M.alter (mapRemove sock) addr (byAddr t)
                  , numConns = numConns t - 1
                  }
  where
    -- first case should really not happen
    mapRemove _    Nothing  = Nothing
    mapRemove sock (Just l) =
        case filter (\(tid',s,_,_) -> tid' /= tid && s /= sock) l of
            [] -> Nothing
            l' -> Just l'

countTableAddrs :: SockAddr -> Table -> Int
countTableAddrs sockAddr =
    maybe 0 length . M.lookup sockAddr . byAddr

listenerCreate :: Socket
               -> FireWall
               -> (Socket -> SockAddr -> IO (Either String (a, IO ()))) -- ^ on open callback
               -> Listener a
listenerCreate sock fw onOpen =
    Listener sock fw onOpen

handlerCreate :: (a -> IO ())   -- ^ running callback
              -> IO (Handler a) 
handlerCreate run =
    Handler <$> newMVar []
            <*> newMVar newTable 
            <*> pure run

handlerAddListener :: Handler a
                   -> Listener a
                   -> IO (Async ())
handlerAddListener handler listener = do
    modifyMVar_ (listeners handler) $ \l -> return (listener : l)
    a <- async $ forever $ do
            catch (listenerAccept listener handler)
                  (\e -> putStrLn ("accepting exception: " ++ show (e :: SomeException)))
            return ()
    return a

listenerAccept :: Listener a
               -> Handler a
               -> IO ()
listenerAccept listener handler = do
    (clientSock, clientAddr) <- accept (listeningSocket listener)
    nbClientsAlready         <- grab (table handler) (return . countTableAddrs clientAddr)
    case checkFirewall (firewall listener) nbClientsAlready clientAddr of
        FireWallDenied _reason -> shutdown clientSock ShutdownBoth
        FireWallAuthorized     -> do
            clientComm    <- newEmptyMVar
            -- start the thread handling this specific client
            clientHandler <- forkIO $ do
                r <- (onConnect listener) clientSock clientAddr
                case r of
                    Left _ -> do
                        shutdown clientSock ShutdownBoth
                    Right (a, onClose) -> do
                        tId <- readMVar clientComm
                        let tableElem = (tId, clientSock, clientAddr, onClose)
                        modifyMVar_ (table handler) (return . insertTable tableElem)
                        catch ((runner handler) a >> onClose >> remove tId) $ \(_exn :: IOException) -> onClose >> remove tId
            putMVar clientComm clientHandler
  where
    remove tid = modifyMVar_ (table handler) (return . removeTableTid tid)
    checkFirewall fw nbClients clientAddr
        | (whitelist fw) clientAddr = FireWallAuthorized
        | (blacklist fw) clientAddr = FireWallDenied BlackListed
        | nbClients < 8             = FireWallAuthorized
        | otherwise                 = FireWallDenied TooManyClients
