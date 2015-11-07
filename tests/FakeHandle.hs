module FakeHandle where

import           Control.Concurrent.MVar
import           Control.Concurrent.Chan
import qualified Data.ByteString as B
import           Network.LSP (Backend(..))

type Popper = Chan B.ByteString

data FakeHandle = FakeHandle ReadChannel (MVar (Maybe Popper))

instance Backend FakeHandle where
    backendRecv = readFakeHandle
    backendSend = writeFakeHandle

data ReadChannel = ReadChannel
    { readBuffered :: MVar B.ByteString
    , readPopper   :: Popper
    }

newFakeHandle :: IO FakeHandle
newFakeHandle = do
    rc <- ReadChannel <$> newMVar B.empty <*> newChan
    FakeHandle rc <$> newMVar Nothing

pipeFakeHandle :: FakeHandle -> FakeHandle -> IO ()
pipeFakeHandle (FakeHandle c1 pop1) (FakeHandle c2 pop2) = do
    modifyMVar_ pop1 $ \_ -> return $ Just $ readPopper c2
    modifyMVar_ pop2 $ \_ -> return $ Just $ readPopper c1

readFakeHandle :: FakeHandle -> Int -> IO B.ByteString
readFakeHandle (FakeHandle rc _) n = do
    bs1 <- modifyMVar (readBuffered rc) $ \b ->
        if B.length b >= n
            then do let (b1, b2) = B.splitAt n b
                    return (b2, b1)
            else return (B.empty, b)
    if B.length bs1 < n
        then B.concat . ((:) bs1) <$> pull (n - B.length bs1)
        else return bs1
  where
    pull len
        | len == 0  = return []
        | otherwise = do
            b <- readChan (readPopper rc)
            if B.length b > len
                then do
                    let (b1, b2) = B.splitAt len b
                    -- old should be empty, but just use B.append for safety
                    modifyMVar_ (readBuffered rc) $ \old -> return (old `B.append` b2)
                    return [b1]
                else do
                    ((:) b) <$> pull (len - B.length b)

writeFakeHandle :: FakeHandle -> B.ByteString -> IO ()
writeFakeHandle (FakeHandle _ varPush) b =
    modifyMVar_ varPush $ \mPush ->
        case mPush of
            Nothing   -> error "invalid fake handle: write side not connected"
            Just push -> writeChan push b >> return mPush
