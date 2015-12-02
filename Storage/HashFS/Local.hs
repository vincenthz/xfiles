module Storage.HashFS.Local
    where

import Data.Word (Word64)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Control.Applicative
import Control.Monad (when, unless, foldM, filterM, forM_)
import Control.Monad.Trans
import Control.Monad.Reader (ask)
import Storage.HashFS.Types
import Storage.HashFS.Hasher
import Storage.HashFS.Path
import Storage.HashFS.Utils
import Storage.HashFS.IO
import Storage.Utils
import System.IO hiding (readFile)
import Prelude hiding (readFile)
import Data.Time.Clock.POSIX (POSIXTime)

import System.Directory (createDirectoryIfMissing, removeFile, doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>), dropTrailingPathSeparator, dropFileName)
import System.Posix.Files hiding (isDirectory)


-- | Create a configuration
makeConf :: HashAlgorithm h
         => [Int]
         -> Hasher h
         -> OutputDesc
         -> FilePath
         -> HashFSConf h
makeConf depth hasher oDesc rootFs =
    HashFSConf { hashfsDepth      = depth
               , hashfsHash       = hasher
               , hashfsOutputDesc = oDesc
               , hashfsRoot       = rootFs
               }

makeConfContext :: HashAlgorithm h
                => [Int]
                -> OutputDesc
                -> FilePath
                -> HashFSConf h
makeConfContext depth oDesc rootFs =
    HashFSConf { hashfsDepth      = depth
               , hashfsHash       = hasherInitContext
               , hashfsOutputDesc = oDesc
               , hashfsRoot       = rootFs
               }

-- | Create a configuration with SHA256 as the Hash
makeConfSHA512 :: [Int] -> OutputDesc -> FilePath -> HashFSConf SHA512
makeConfSHA512 depth = makeConf depth hasherInitContext

-- | Create a configuration with SHA256 as the Hash
makeConfSHA256 :: [Int] -> OutputDesc -> FilePath -> HashFSConf SHA256
makeConfSHA256 depth = makeConf depth hasherInitContext

-- | Compute the digest according to the HashFSConf of any file
computeHash :: HashAlgorithm h => HashFSConf h -> FilePath -> IO (Digest h)
computeHash conf file =
    hashCompute (hashfsHash conf) . L.toChunks <$> liftIO (L.readFile file)

-- | make sure the hierarchy required to put the file is created
createHier :: HashAlgorithm h => HashFSConf h -> Digest h -> IO ()
createHier conf hsh = do
    let spath = getDigestSplitPath conf hsh
    -- FIXME do it recursively, as it depends on the configured depth
        destPath = getPath conf hsh
    when (length spath > 1) $ liftIO $ createDirectoryIfMissing True (dropFileName $ dropTrailingPathSeparator destPath)

-- | Copy a file to the necessary digest
copyFileToHash :: HashAlgorithm h
               => HashFSConf h
               -> ((Context h -> ByteString -> IO (Context h)) -> Context h -> IO (Context h))
               -> IO (Digest h)
copyFileToHash conf blocksAcc = do
    (tmpFile, tmpHandle) <- tmpfilePath conf
    hsh  <- hashFinalize <$> blocksAcc (copyAndHash tmpHandle) (hashInitialize (hashfsHash conf))
    liftIO $ hClose tmpHandle
    existsAlready <- exists conf hsh
    if existsAlready
        then removeFile tmpFile >> return hsh
        else do
            createHier conf hsh
            let destPath = getPath conf hsh
            liftIO $ rename tmpFile destPath
            return hsh
  where
    copyAndHash outHandle hashCtx block =
        B.hPut outHandle block >> return (hashUpdate hashCtx block)

hashFileDataReader :: HashAlgorithm h => HashFSConf h -> Digest h -> IO DataReader
hashFileDataReader conf digest = filepathDataReader (getPath conf digest)

hashFileDataWriter :: HashAlgorithm h => HashFSConf h -> IO (DataWriterDigest h)
hashFileDataWriter conf = do
    (tmpFile, tmpHandle) <- tmpfilePath conf
    let put bs = B.hPut tmpHandle bs
        done mComputedDigest = do
            hClose tmpHandle
            -- remove the tmpfile if:
            --  * file is wrongly computed (digest is Nothing)
            --  * destination file already exists
            -- otherwise rename the file to the destination path
            case mComputedDigest of
                Nothing             -> removeFile tmpFile
                Just computedDigest -> do
                    existsAlready <- exists conf computedDigest
                    if existsAlready
                        then removeFile tmpFile >> return ()
                        else do
                            createHier conf computedDigest
                            let destPath = getPath conf computedDigest
                            liftIO $ rename tmpFile destPath
                            return ()
    return $ DataWriterDigest put done

-- | initialize ! :)
initialize :: HashAlgorithm h => HashFSConf h -> IO ()
initialize conf = do
    let c = hashfsRoot conf
    rootExists <- liftIO $ doesDirectoryExist c
    unless rootExists $ error "hashfs root directory doesn't exist"
    isNull <- null . filter metaDirs <$> liftIO (getDirectoryContents c)
    unless isNull $ error "hashfs root directory isn't empty"
    return ()
  where metaDirs = not . flip elem [".", ".."]

data ImportType = ImportCopy | ImportSymlink | ImportHardlink
    deriving (Show,Eq)

-- | import a file into a Hash Filesystem, and return the new digest
importFile :: HashAlgorithm h => HashFSConf h -> ImportType -> FilePath -> IO (Digest h)
importFile conf itype file = do
    case itype of
        ImportCopy     -> do
            c <- liftIO $ L.readFile file
            copyFileToHash conf (\f ctx -> foldM f ctx $ L.toChunks c)
        ImportSymlink  -> doImportLink True
        ImportHardlink -> doImportLink False
  where doImportLink isSymlink = do
            digest   <- computeHash conf file
            let destPath = getPath conf digest
            createHier conf digest
            liftIO $ if isSymlink
                then createSymbolicLink file destPath
                else createLink file destPath
            return digest

-- | import a file to a specified Digest value. use with care.
importFileAt :: HashAlgorithm h => HashFSConf h -> FilePath -> Digest h -> IO ()
importFileAt conf file digest = do
    existsAlready <- exists conf digest
    let fp = getPath conf digest
    (tmpFile, tmpHandle) <- tmpfilePath conf
    liftIO $ hClose tmpHandle
    if existsAlready
        then return ()
        else L.readFile file >>= L.writeFile tmpFile >> rename tmpFile fp

-- | Delete a file from the hash filesystem
deleteFile :: HashAlgorithm h => HashFSConf h -> Digest h -> IO ()
deleteFile conf digest = removeFile (getPath conf digest)

-- | Verify that a specific digest is backed by
-- a valid file where the hash exists
verify :: HashAlgorithm h => HashFSConf h -> Digest h -> IO Bool
verify conf digest = do
    digestGot <- onDigestFile conf digest (catchIO . (\f -> doHash <$> L.readFile f))
    return $ Just digest == digestGot
  where doHash = hashCompute (hashfsHash conf) . L.toChunks

-- | compute something on a filepath associated with a digest
onDigestFile :: HashAlgorithm h => HashFSConf h -> Digest h -> (FilePath -> IO a) -> IO a
onDigestFile conf digest f = f (getPath conf digest)

-- | Check that a digest exists in the filesystem
exists :: HashAlgorithm h => HashFSConf h -> Digest h -> IO Bool
exists conf digest = onDigestFile conf digest (doesFileExist)

-- | try to read a file backed by a certain digest
readFile :: HashAlgorithm h => HashFSConf h -> Digest h -> IO (Maybe L.ByteString)
readFile conf digest = onDigestFile conf digest (catchIO . L.readFile)

-- | get information about a specific Digest, namely size and mtime
readInfo :: HashAlgorithm h => HashFSConf h -> Digest h -> IO (Maybe (FileSize, ModificationTime))
readInfo conf digest = onDigestFile conf digest (\path -> catchIO (getFileInfo path))

-- | Take a list of prefixes and returns chunks in this prefix path directory
iterateFiles :: HashAlgorithm h => HashFSConf h -> (Digest h -> IO ()) -> IO ()
iterateFiles conf callback =
    loop (maybe (return ()) callback . inputd) (hashfsDepth conf) fullLength "" (hashfsRoot conf)
  where
    loop :: (String -> IO ()) -> [Int] -> Int -> String -> FilePath -> IO ()
    loop f (n:ns) i pre dir = do
        ents <- getDirectoryContents dir
        let validEnts = filter ((== n) . length) ents
        validEntsDir <- filterM (liftIO . doesDirectoryExist . (dir </>)) validEnts
        mapM_ (\p -> loop f ns i (pre ++ p) (dir </> p)) validEntsDir
    loop f []     _ pre dir = do
        ents <- getDirectoryContents dir
        forM_ ents $ \r -> f (pre ++ r)

    fullLength :: Int
    fullLength = length $ outputDigest (hashfsOutputDesc conf) $ hashGetDummy (hashfsHash conf)

    --inputd :: HashAlgorithm h => HashFSConf h -> String -> Maybe (Digest h)
    inputd s = inputDigest (hashfsOutputDesc conf) s
