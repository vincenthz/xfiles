module Tools.ChronoFs.Marshall where

import Tools.ChronoFs.Types
import Data.Serialize.Put
import Data.Serialize.Get
import qualified Data.ByteString as B
import Filesystem.Path.Rules as FP (encode, decode, posix)
import Control.Applicative
import Control.Monad


marshallEnt :: Ent -> B.ByteString
marshallEnt ent = runPut $ do
    putType $ entType ent
    putWord8 0xff
    putWord16le (fromIntegral $ B.length nameMarshalled)
    putWord32le (fromIntegral $ entPerms ent)
    putWord64le (truncate $ entMTime ent)
    putWord64le (truncate $ entCTime ent)
    case entType ent of
        EntLink -> linkAsHash (entHash ent)
        _       -> putHash (entHash ent)
    putByteString nameMarshalled
  where nameMarshalled :: B.ByteString
        nameMarshalled = FP.encode FP.posix $ entName ent

        putHash (Hash b)    = putByteString b
        linkAsHash (Hash b) = putByteString btrans
          where btrans = runPut (putWord16le (fromIntegral $ B.length b) >> putByteString b)

        putType EntDir  = putWord8 1
        putType EntFile = putWord8 2
        putType EntLink = putWord8 3

unmarshallEnt :: B.ByteString -> Either String Ent
unmarshallEnt bs = runGet parseEnt bs

unmarshallEnts :: B.ByteString -> Either String [Ent]
unmarshallEnts bs = runGet (loopParseEnt) bs
  where loopParseEnt = do
            e <- isEmpty
            if e
                then return []
                else liftM2 (:) parseEnt loopParseEnt

parseEnt :: Get Ent
parseEnt = do
    t <- getType
    _ <- getWord8
    len <- getWord16le
    perms <- getWord32le
    mtime <- getWord64le
    ctime <- getWord64le
    h <- if t == EntLink then getLinkAsHash else getHash
    filename <- FP.decode FP.posix <$> getByteString (fromIntegral len)
    return $ Ent { entType  = t
                 , entPerms = fromIntegral perms
                 , entMTime = realToFrac mtime
                 , entCTime = realToFrac ctime
                 , entHash  = h
                 , entName  = filename
                 }
  where getType = toTy <$> getWord8
        getHash = Hash <$> getByteString 64
        getLinkAsHash = Hash <$> (getWord16le >>= getByteString . fromIntegral)

        toTy 1 = EntDir
        toTy 2 = EntFile
        toTy 3 = EntLink
        toTy n = error ("unknown entity type: " ++ show n)
