{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Int
import           Data.Word
import           Network.Socket hiding (send,recv)
import           System.Timeout
import           System.Environment
import           System.FilePath
import           System.Directory
import           System.Exit
import           Control.Monad
import           Control.Concurrent
import           Control.Arrow (second)
import           Text.Read
import           Data.Maybe
import           Data.List (find)
import           Data.Monoid
import           Crypto.Error

import qualified Data.ByteArray.Encoding as B
import qualified Data.ByteArray as B
import qualified Data.ByteString as B (readFile)
import qualified Data.ByteString.Char8 as BC

import           Storage.HashFS
import           Storage.HashFS.Client
import           Storage.HashFS.Protocol
import           Storage.HashFS.IO
import           Storage.HashFS.Utils
import           Storage.Utils

import           Console.Options
import           Console.Display

import           Data.Hourglass

import           Database.HDBC
import           Database.HDBC.Sqlite3

import qualified Data.Map as M

data IndexType :: *

data IGroup -- :: IndexType
data ITag -- :: IndexType
data IData --  :: IndexType
data ICategory -- :: IndexType

newtype Index a = Index Int64
    deriving (Show,Eq,Ord)

toIndex :: SqlValue -> Index a
toIndex (SqlInt64 v) = Index v
toIndex s = error ("cannot c0nvert to index : " ++ show s)

doImport _ path context = do
    -- find provider
    let !prov = maybe (error "cannot find imports provider") id $ findProviderByName context "imports"
        !meta = head $ contextMetaviders context

    dat <- readAllDb path
    putStrLn (show (length dat) ++ " records found")

    forM_ (zip [1..] dat) $ \(idx :: Int, d) -> do
        let fp = let (dir,file) = splitAt 2 (datHash d)
                  in path </> dir </> file
        let dinfo = DataInfo
                    { dataSize     = datSize d
                    , dataDate     = Just $ Elapsed $ Seconds $ fromIntegral $ datMtime d
                    , dataDirName  = Nothing
                    , dataFileName = Nothing
                    }
        let tags = map toTag $ aTags $ datAssociated d
        (importedAlready, dig) <- importInto (Just (meta, dinfo, tags)) prov ImportCopy fp
        putStrLn $ show idx ++ "/" ++ show (length dat) ++ " => " ++ (if importedAlready then "!" else "+")++ " " ++ show dig
        return ()

    mapM_ (putStrLn . show) dat
    return ()
  where
    toTag (t, "personal") = Tag Nothing t
    toTag (t, "person")   = Tag (Just Person) t
    toTag (t, "location") = Tag (Just Location) t
    toTag (t, _)          = Tag Nothing t

readAllDb path = do
    let dbPath = path </> "quarry.db"

    exists <- doesFileExist dbPath
    unless exists $ error ("no quarry.db file found at " ++ path)

    conn <- connectSqlite3 dbPath

    tables <- getTables conn
    putStrLn $ show tables

    cats <- M.fromList . map toCat <$> quickQuery' conn "SELECT * from category" []

    grps <-
        if | "group" `elem` tables -> M.fromList . map toGrp <$> quickQuery' conn "SELECT * from grp" []
           | otherwise             -> return M.empty

    tags <- M.fromList . map toTag <$> quickQuery' conn "SELECT * from tag" []

    tagmap <- map toTagMap <$> quickQuery' conn "SELECT * from tagmap" []

    grpmap <-
        if | "group" `elem` tables -> map toGrpMap <$> quickQuery' conn "SELECT * from grpmap" []
           | otherwise             -> return []

    dats <- map toData <$> quickQuery' conn "SELECT * from data" []

    return $ flip map dats $ \(idData, dat) ->
        let cat  = resolve "category" cats (datCategory dat)
            ts = map (second (resolve "category" cats) . resolve "tag" tags) $ findAll idData tagmap
            gs = map (resolve "group" grps) $ findAllRev idData grpmap
         in dat { datAssociated = Associated cat ts gs }

  where
    resolve :: String -> M.Map (Index a) b -> Index a -> b
    resolve s l i = maybe (error ("cannot resolve in" ++ s)) id $ M.lookup i l

    toCat :: [SqlValue] -> (Index ICategory, String)
    toCat [v, n@(SqlByteString _), _] = (toIndex v,fromSql n)
    toCat l = error ("cannot transform to category: " ++ show l)

    toGrp :: [SqlValue] -> (Index IGroup, String)
    toGrp [v, n@(SqlByteString _), _] = (toIndex v,fromSql n)
    toGrp l = error ("cannot transform to category: " ++ show l)

    toTag :: [SqlValue] -> (Index ITag, (String, Index ICategory))
    toTag [t, n@(SqlByteString _), cid] = (toIndex t,(fromSql n, toIndex cid))
    toTag l = error ("cannot transform to tag: " ++ show l)

    toTagMap :: [SqlValue] -> (Index IData, Index ITag)
    toTagMap [v, v2] = (toIndex v,toIndex v2)
    toTagMap l = error ("cannot transform to tagmap: " ++ show l)

    toGrpMap :: [SqlValue] -> (Index IGroup, Index IData)
    toGrpMap [v, v2, _] = (toIndex v,toIndex v2)
    toGrpMap l = error ("cannot transform to grpmap: " ++ show l)

    toData :: [SqlValue] -> (Index IData, Dat ())
    toData [did,h@(SqlByteString _),cid,desc,SqlInt64 t1,SqlInt64 t2,SqlInt64 t3,SqlInt64 t4
           ,dir@(SqlByteString _), file@(SqlByteString _),SqlInt64 v3] =
        (toIndex did, Dat (fromSql h) (toIndex cid) (toDesc desc) (fromIntegral t1) (fromIntegral t2) (fromIntegral t3) (fromIntegral t4)
            (fromSql dir) (fromSql file) (fromIntegral v3) ())
    toData l = error ("cannot transform to data: " ++ show l)

    toDesc (SqlNull)           = ""
    toDesc b@(SqlByteString _) = fromSql b
    toDesc l                   = error ("toDesc: invalid format: " ++ show l)

    findAll :: Eq a => a -> [(a,b)] -> [b]
    findAll v = map snd . filter ((==) v . fst)

    findAllRev :: Eq b => b -> [(a,b)] -> [a]
    findAllRev v = map fst . filter ((==) v . snd)

-- "CREATE TABLE data (id INTEGER PRIMARY KEY
{-
, hash VARCHAR(80) NOT NULL
, category INTEGER NOT NULL
, description VARCHAR(1024)
, size WORD64
, mtime WORD64
, date WORD64, itime WORD64, dirname VARCHAR(4096), filename VARCHAR(1024), filetype INTEGER)"
-}
data Dat a = Dat
    { datHash        :: String
    , datCategory    :: Index ICategory
    , datDescription :: String
    , datSize        :: Word64
    , datMtime       :: Word64
    , datDate        :: Word64
    , datITime       :: Word64
    , datDirName     :: String
    , datFilename    :: String
    , datFiletype    :: Int
    , datAssociated  :: a
    } deriving (Show,Eq)

data Associated = Associated
    { aCat    :: String
    , aTags   :: [(String, String {- cat -})]
    , aGroups :: [String]
    } deriving (Show,Eq)

main :: IO ()
main = defaultMain $ do
    programName "importer"
    programDescription "quarry importer"

    pathFlag <- flagParam (FlagLong "path" <> FlagShort 'p' <> FlagDescription "path")
                          (FlagRequired Right)

    action $ \toParam -> do
        let path = maybe (error "path not specified") id $ toParam pathFlag
        disp <- displayInit
        withConfig (doImport disp path)
