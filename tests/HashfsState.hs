{-# LANGUAGE FlexibleInstances #-}
module HashfsState where

import           Control.Applicative
import           Control.Monad

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Monadic

import           Storage.HashFS.Meta
import           Storage.HashFS
import           System.Directory
import           Crypto.Hash
import           Data.List
import           Data.Function (on)

import           Data.Hourglass
import           Data.Word

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M

data State h = State
    { stateContents   :: [Digest h]
    , stateTags       :: [Tag]
    , stateAssociates :: M.Map (Digest h) (DataInfo, [Tag])
    } deriving (Show,Eq)

instance HashAlgorithm h => Arbitrary (State h) where
    arbitrary = do
        digests <- choose (1,351) >>= flip replicateM (hash . BC.pack <$> replicateM 8 arbitrary)
        tags    <- choose (1,124) >>= flip replicateM arbitrary
        ass     <- forM digests $ \dg -> do
                    di     <- arbitrary
                    dgTags <- nub <$> (choose (0,4) >>= flip replicateM (elements tags))
                    return (dg, (di, dgTags))

        return $ State digests tags (M.fromList ass)

newState :: HashAlgorithm h => h -> Gen (State h)
newState _ = arbitrary

instance Arbitrary Tag where
    arbitrary =
        Tag <$> elements [Just Other, Just Person, Just Location, Just Group]
            <*> elements arbitraryTag

forEachDigestsAndData :: HashAlgorithm h => State h -> ((Digest h, (DataInfo, [Tag])) -> IO a) -> IO ()
forEachDigestsAndData st f = forM_ (M.toList $ stateAssociates st) f

forEachTags :: HashAlgorithm h => State h -> (Tag -> IO a) -> IO ()
forEachTags st f = forM_ (stateTags st) f

forEachDigests :: HashAlgorithm h => State h -> (Digest h -> IO a) -> IO ()
forEachDigests st f = forM_ (stateContents st) f

stateGetUntagged :: State h -> [Digest h]
stateGetUntagged st =
    map fst $ filter (\(_,(_,ts)) -> null ts) $ M.toList $ stateAssociates st

stateGetMultiTags :: State h -> [([Tag], (Int, [Digest h]))]
stateGetMultiTags st =
      sortBy (compare `on` fst . snd)
    $ map (\x -> (snd $ head x, (length x, map fst x))) -- head is safe here, as group doesn't return empty list
    $ groupBy ((==) `on` snd)
    $ sortBy (compare `on` snd)
    $ map (\(d, (_,ts)) -> (d,ts))
    $ filter (\(_,(_,ts)) -> length ts > 1)
    $ M.toList $ stateAssociates st

stateFindMultiTags :: State h -> [Tag] -> [Digest h]
stateFindMultiTags st tags =
      map fst
    $ filter (\(_,(_,ts)) -> intersect tags ts == tags)
    $ M.toList $ stateAssociates st

stateGetTagOf :: State h -> Digest h -> [Tag]
stateGetTagOf st dg =
    maybe [] snd $ M.lookup dg $ stateAssociates st

stateGetDigestTaggedWith :: State h -> Tag -> [Digest h]
stateGetDigestTaggedWith st tag =
    map fst $ filter (\(_,(_,ts)) -> tag `elem` ts) $ M.toList $ stateAssociates st

arbitraryFileContent :: [String]
arbitraryFileContent = [ [x,y] | x <- ['a'..'b'], y <- ['a'..'z'] ]

arbitraryDigest :: HashAlgorithm h => h -> [Digest h]
arbitraryDigest hashAlg = map (hashWith hashAlg . BC.pack) arbitraryFileContent

arbitraryTag :: [String]
arbitraryTag =
    ["foo","bar","baz","time","eol","a","b","c", "a'b'c", "he huk neh kak jajvak" ]
    ++ map (("tag-" ++) . show) [15..124 :: Int]
    ++ map (("group-" ++) . show) [14..84 :: Int]

arbitraryFilesize :: [Word64]
arbitraryFilesize = [1, 1248908, 4129124, 2901, 24180421490, 12389, 4125]

instance Arbitrary DataInfo where
    arbitrary = do
        filename <- elements [Nothing, Just "abc.jpg", Just "xoxo"]
        dt       <- (Just . toElapsed) <$> elements [1, 2901, 8249414]
        sz       <- choose (1, maximum arbitraryFilesize + 20)
        return $ DataInfo sz dt Nothing filename

toElapsed = Elapsed . Seconds

data Query = Query
    { queryString :: String
    , queryFilter :: DataInfo -> [Tag] -> Bool
    }

data QueryType = QueryFilesize | QueryRating
    deriving (Show,Eq,Enum,Bounded)

instance Arbitrary Query where
    arbitrary = do
        ty <- elements [minBound..]
        case ty of
            QueryFilesize -> do
                (str, fct) <- genNumQuery $ fromIntegral (maximum arbitraryFilesize + 1)
                return $ Query { queryString = "filesize " ++ str
                               , queryFilter = \dt _ -> fct . fromIntegral . dataSize $ dt
                               }
            QueryRating -> do
                -- FIXME using dataSize .. as DataInfo doesn't have rating
                (str, fct) <- genNumQuery 5
                return $ Query { queryString = "rating " ++ str
                               , queryFilter = \dt _ -> fct . fromIntegral . dataSize $ dt
                               }

-- :: [Query] ->

genNumQuery :: Integer -> Gen (String, Integer -> Bool)
genNumQuery maxN = do
    op       <- elements [GT,EQ,LT]
    included <- elements [False,True]
    n        <- choose (1, maxN)
    let (opStr, fct) = case (op, included) of
                    (GT, True)  -> (">=", (>=) n)
                    (GT, False) -> (">", (>) n)
                    (LT, True)  -> ("<=", (<=) n)
                    (LT, False) -> ("<", (<) n)
                    (EQ, _)     -> ("==", (==) n)
    return (opStr ++ " " ++ show n, fct)
