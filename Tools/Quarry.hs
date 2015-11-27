-- Quarry main module
--
module Tools.Quarry
    ( initialize
    -- * Types
    , KeyCategory
    , KeyTag
    , TagName
    , Tag(..)
    , ImportType(..)
    , DataCategory(..)
    , QuarryConfig
    , QuarryFileType(..)
    , QuarryM
    , FileFormat(..)
    -- * Methods
    , QuarryDigest
    , QuarryDigestInfo(..)
    --, getQuarryDigestType
    , getQuarryFileType
    , getQuarryDigestInfo
    , runQuarry
    , importFile
    , updateDigest
    , resolveDigest
    , resolveTag
    --, getDigestPath
    , getCategoryTable
    , findDigestWithTags
    , readDigest
    , findTags
    , exist
    , computeDigest
    , addCategory
    , QuarryInfo(..)
    , getInfo
    ) where

import Storage.HashFS (ImportType(..))
import qualified Storage.HashFS as HFS
import System.Posix.Files
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Crypto.Hash (SHA512)

import System.FilePath
import System.Directory
import Data.Word

import Storage.Utils

import Database.HDBC.Sqlite3 (connectSqlite3)

import Tools.Quarry.Types
import Tools.Quarry.Cache
import Tools.Quarry.Config
import Tools.Quarry.Monad
import Tools.Quarry.DB
import Tools.Quarry.DBHelper
import Data.FileFormat

-- | Try to transform a string into a digest
readDigest :: String -> Maybe QuarryDigest
readDigest s = HFS.inputDigest HFS.OutputHex s

-- | Run an HashFS monad operation on top of Quarry.
runHFS :: (HFS.Provider SHA512 -> IO a) -> QuarryM a
runHFS f = ask >>= \conf -> liftIO $ f (hashfsConf conf)

--getRootPath = runHFS (HFS.hashfsRoot <$> ask)

-- | Check if the digest already exists in the database
exist :: QuarryDigest -> QuarryM Bool
exist digest = runHFS $ \conf -> HFS.exists [conf] digest

-- | Compute the digest associated with a file
computeDigest :: FilePath -> QuarryM QuarryDigest
computeDigest file = liftIO $ HFS.hashFileContext file -- runHFS (\conf -> HFS.fileHashContext conf file)

-- | initialize a new quarry database object.
--
-- The user has the choice to init the storage.
-- if the storage is already initialized then an error will be returned,
-- otherwise it will errors out if the path doesn't looks like a valid quarry database.
initialize :: Bool            -- ^ if we initialize the storage or not
           -> FilePath        -- ^ The filepath of the database
           -> IO QuarryConfig -- ^ the config read
initialize wantNew root = do
    dirExist <- doesDirectoryExist root
    hasDb <- doesFileExist (dbFile root)
    provider <- if wantNew
        then do when hasDb $ error "look like it's already initialized"
                HFS.initializeLocally "quarry" root
        else do when (not hasDb) $ error "look like no DB"
                return $ HFS.Provider "quarry" HFS.ReadWrite (HFS.ProviderLocal $ HFS.makeConfContext [2] HFS.OutputBase32 root)
    conn <- connectSqlite3 (dbFile root)
    when wantNew $ dbCreateTables conn
    cache <- emptyCache
    return $ QuarryConfig { connection = conn, hashfsConf = provider, cacheTags = cache }
  --where quarryHashFSConf = HFS.makeConfSHA512 [2] HFS.OutputHex root

-- | Import an element into quarry and returns the digest associated
-- and if the element has been created or updated.
importFile :: ImportType      -- ^ Whether to copy/symlink/hardlink ..
           -> DataCategory    -- ^ Category of data (video,book,..)
           -> Maybe ModificationTime -- ^ An optional associated date
           -> Maybe FilePath  -- ^ Alternative name to import the file to
           -> [Tag]           -- ^ Tags to add to this file
           -> FilePath        -- ^ Path to the file
           -> QuarryM (QuarryDigest,Bool)
importFile itype dataCat mDate mFilename tags rfile = do
    current <- liftIO getCurrentDirectory
    let file = if isRelative rfile then current </> rfile else rfile
    info   <- liftIO $ getFileInfo file
    digest <- runHFS $ \conf -> HFS.importInto conf itype file
    key    <- dbResolveDigest digest
    case key of
        Nothing -> do
            ty <- getQuarryFileType file
            k  <- dbAddFile digest dataCat (maybe file id mFilename) mDate info ty
            when (not $ null tags) $ do
                mapM_ (dbCreateTag >=> dbAddTag k) tags
            dbCommit
            return (digest, True)
        Just _ -> return (digest, False)

{-
getQuarryDigestType :: QuarryDigest -> QuarryM (QuarryFileType, FileFormat)
getQuarryDigestType digest = do
    p  <- getDigestPath digest
    ff <- liftIO $ getFileformat p
    return (toQuarryFileType ff, ff)
-}

-- | Return the associated data with a quarry digest
getQuarryDigestInfo :: QuarryDigest -> QuarryM (Maybe QuarryDigestInfo)
getQuarryDigestInfo digest =
    dbGetInfo digest

-- | Return the associated filetype with a file
getQuarryFileType :: FilePath -> QuarryM QuarryFileType
getQuarryFileType path = liftIO (toQuarryFileType <$> getFileformat path)

toQuarryFileType :: FileFormat -> QuarryFileType
toQuarryFileType ft = case ft of
    FT_JPEG   -> QuarryTypeImage
    FT_JPEG_EXIF -> QuarryTypeImage
    FT_JFIF_EXIF -> QuarryTypeImage
    FT_PNG    -> QuarryTypeImage
    FT_PDF _  -> QuarryTypeDocument
    FT_MP3    -> QuarryTypeSound
    FT_OGG    -> QuarryTypeSound
    --FT_RIFF   -> QuarryTypeImage -- webp .. could be video !
    FT_AVI    -> QuarryTypeVideo
    FT_Text   -> QuarryTypeDocument
    _         -> QuarryTypeUnknown

-- | Update a specific digest by adding tags and/or removing tags
updateDigest :: QuarryDigest -- ^ Digest to update
             -> [Tag]        -- ^ Tags to add
             -> [Tag]        -- ^ Tags to remove
             -> QuarryM ()
updateDigest digest addTags delTags = do
    mfk <- dbResolveDigest digest
    case mfk of
        Nothing -> error "digest not found"
        Just fk -> do
            mapM_ (dbCreateTag >=> dbAddTag fk) addTags
            mapM_ (dbFindTag >=> maybe (return ()) (dbRemoveTag fk)) delTags
            dbCommit

-- | Try to resolve a digest into a key data
resolveDigest :: QuarryDigest -> QuarryM (Maybe KeyData)
resolveDigest digest = dbResolveDigest digest

-- | Try to resolve a tag name to a tag
resolveTag :: Either TagName Tag -> QuarryM (Maybe Tag)
resolveTag (Left tname) = do
    r <- dbFindTagsMatching (Just tname) Nothing
    case r of
        [(cat,tname2)] -> dbResolveKeyCategory cat >>= \c -> return $ Just $ Tag { tagCat = c, tagName = tname2 }
        _              -> return Nothing
resolveTag (Right tag) = return $ Just tag

-- | Return the path where the digest is stored
--getDigestPath :: QuarryDigest -> QuarryM FilePath
--getDigestPath d = runHFS (\conf -> return $ HFS.getPath conf d)

findDigestWithTags :: [Tag] -> QuarryM [QuarryDigest]
findDigestWithTags tags = dbFindWithTags tags

-- | find tags with specific queries
--
-- at the moment only 'starting by' query supported
findTags :: Maybe String -> Maybe Category -> QuarryM [(KeyCategory, TagName)]
findTags s mcat =
    -- need ending by, contains, etc..
    dbFindTagsMatching s mcat

-- | Return the category table contents
--
-- >    | Key | Name | User |
getCategoryTable :: QuarryM [(KeyCategory,(Category,Bool))]
getCategoryTable = dbGetCategories

-- | Add a new category
addCategory :: Category -> QuarryM ()
addCategory cat = do
    _ <- dbCreateCategory cat
    dbCommit
    return ()

data QuarryInfo = QuarryInfo
    { infoNFile     :: Word64
    , infoNTag      :: Word64
    , infoNCategory :: Word64
    } deriving (Show,Eq)

-- | Return generic information about the quarry database.
--
-- For now, returns number of elements, tags, and categories.
getInfo :: QuarryM QuarryInfo
getInfo = withDB $ \conn -> liftIO $
    QuarryInfo <$> getCount conn tableData Nothing
               <*> getCount conn tableTag Nothing
               <*> getCount conn tableCategory Nothing

--digestOfKeys :: [DataKey] -> QuarryM [QuarryDigest]
--digestOfKeys fks = mapM dbResolveKey fks

