{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Main (main) where

import           Control.Applicative
import           Control.Monad

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Monadic

import           Storage.HashFS.Meta
import           System.Directory
import           Crypto.Hash
import           Data.List

import           Data.Hourglass

import qualified Data.ByteString.Char8 as BC

data Infinite a = Infinite a (Infinite a)

grab :: Infinite a -> (a, Infinite a)
grab (Infinite a next) = (a, next)

rng :: Int -- initial N
    -> Int -- tweak
    -> Int -- mod
    -> Infinite Int
rng initN tweak m = loop (initN * tweak)
  where
    (a,b) = (2901,643)
    loop n =
        let nextN = (a * n + b) `mod` m
         in Infinite nextN $ loop (nextN * tweak)

newtype SqlFile = SqlFile String
    deriving (Show,Eq,Read)

testDirPrefix :: String
testDirPrefix = "test-sql-tmp"

instance Arbitrary SqlFile where
    arbitrary = SqlFile . ((testDirPrefix ++ "/") ++) <$> replicateM 8 (elements ['a'..'z'])

data SqlHash = forall h . (Show h, HashAlgorithm h) => SqlHash h

instance Show SqlHash where
    show (SqlHash h) = show h

instance Arbitrary SqlHash where
    arbitrary = elements [ SqlHash Blake2s_224
                         , SqlHash Blake2s_256
                         , SqlHash SHA224
                         , SqlHash SHA256
                         , SqlHash SHA512
                         ]

withParameters :: (forall h . HashAlgorithm h => h -> SqlFile -> Int -> IO ()) -> PropertyM IO ()
withParameters f = do
    (SqlHash h, sqlfile@(SqlFile {}), sqlShuffle) <- pick $ (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    run $ do
        createDirectoryIfMissing True testDirPrefix
        f h sqlfile sqlShuffle
    return ()

arbitraryFileContent :: [String]
arbitraryFileContent = [ [x,y] | x <- ['a'..'b'], y <- ['a'..'z'] ]

arbitraryDigest :: HashAlgorithm h => h -> [Digest h]
arbitraryDigest hashAlg = map (hashWith hashAlg . BC.pack) arbitraryFileContent

arbitraryTag :: [Tag]
arbitraryTag = map (Tag (Just Other)) l
  where
    l = ["foo","bar","baz","time","eol","a","b","c"]
        ++ map (("tag-" ++) . show) [15..124 :: Int]
        ++ map (("group-" ++) . show) [14..84 :: Int]

arbitraryDataInfo :: [DataInfo]
arbitraryDataInfo =
    [ DataInfo sz dt Nothing filename
    | filename <- [Nothing, Just "abc.jpg", Just "xoxo"]
    , dt       <- map (Just . toElapsed) [1, 2901, 8249414]
    , sz       <- [1, 1248908, 2901]
    ]
  where
    toElapsed = Elapsed . Seconds

associateTags :: Int -> [Digest h] -> [Tag] -> [(Digest h, DataInfo, [Tag])]
associateTags shuffle ds ts =
    combine (rng shuffle 3 (length tagList)) (shuffleList (rng shuffle 7 124914) ds)
  where
    tagList = shuffleList (rng shuffle 19 24195) ts

    combine _ []     = []
    combine r (d:ds) =
        let (n, r') = grab r
            nbItems = n `mod` 5
            x = n `mod` (length tagList - nbItems)
            (_, tags2) = splitAt x tagList
            di = elemN n arbitraryDataInfo
         in (d, di, take nbItems tags2) : combine r' ds

    elemN n l = l !! (n `mod` length l)

    shuffleList rng l = loop rng [] l
      where
        loop _ acc []  = acc
        loop _ acc [x] = x : acc
        loop r acc l   =
            let (n, r') = grab r
             in case splitAt (n `mod` len) l of
                ([], [])   -> acc
                (x:l1, []) -> loop r' (x : acc) l1
                (l1, x:l2) -> loop r' (x : acc) (l1 ++ l2)
        len = length l

sqlQueriesWork :: HashAlgorithm h => h -> SqlFile -> Int -> IO ()
sqlQueriesWork hashAlg (SqlFile f) shuffle = do
    conn <- either error id <$> metaConnect "sqlite3" f
    let dgs = arbitraryDigest hashAlg

    let ass = associateTags shuffle dgs arbitraryTag
    forM_ ass $ \(dg, di, _) -> do
        metaAddData conn dg di
    forM_ arbitraryTag $ \t -> do
        metaCreateTag conn t
    metaCommit conn

    forM_ ass $ \(d, _, ts) -> do
        mapM_ (metaTag conn d) ts
    metaCommit conn

    forM_ arbitraryTag $ \tag -> do
        found <- metaTagGetDigests conn tag
        let expected = map fst3 $ filter (\(_,_,ts) -> tag `elem` ts) ass
        listCompare ("digest for " ++ show tag ++ " missing ") expected found

    forM_ dgs $ \dg -> do
        found <- metaDigestGetTags conn dg
        let expected = maybe [] thd3 $ find (\(d,_,_) -> d == dg) ass
        listCompare ("tags for digest " ++ show dg ++ " missing") expected found

    notTagged <- metaFindDigestsNotTagged conn
    let expected = map fst3 $ filter (\(_,_,ts) -> null ts) ass
    listCompare ("not tagged missing") expected notTagged

    return ()
  where
    fst3 (a,_,_) = a
    thd3 (_,_,c) = c
    listCompare err expected found
        | sort found /= sort expected = error (err ++ "\ngot     : " ++ show found ++ "\nexpected: " ++ show expected)
        | otherwise = return ()

sqlTests :: [TestTree]
sqlTests =
    [ testProperty "sql-work" (monadicIO $ withParameters sqlQueriesWork)
    ]

main :: IO ()
main = defaultMain $ testGroup "meta"
    [ testGroup "SQL" sqlTests
    ]
