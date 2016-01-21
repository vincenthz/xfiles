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

import qualified Data.ByteString.Char8 as BC

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
arbitraryFileContent = map (replicate 1) ['a'..'z']

arbitraryDigest :: HashAlgorithm h => h -> [Digest h]
arbitraryDigest hashAlg = map (hashWith hashAlg . BC.pack) arbitraryFileContent

arbitraryTag :: [String]
arbitraryTag = ["foo","bar","baz","time","eol","a","b","c"]
    ++ map (("tag-" ++) . show) [15..124 :: Int]
    ++ map (("group-" ++) . show) [14..84 :: Int]

associateTags :: Int -> [Digest h] -> [String] -> [(Digest h, [String])]
associateTags shuffle ds ts =
    combine (shuffle `mod` length tagList) (shuffleList shuffle ds)
  where
    tagList = shuffleList shuffle ts

    combine _ []     = []
    combine n (d:ds) =
        let nextN = (a * n + b + 3) `mod` length tagList
            nbItems = n `mod` 5
            x = n `mod` (length tagList - nbItems)
            (_, r) = splitAt x tagList
         in (d, take nbItems r) : combine nextN ds
      where (a,b) = (2901,643)

    shuffleList initialN l = loop initialN [] l
      where
        loop _ acc []  = acc
        loop _ acc [x] = x : acc
        loop n acc l   =
            let nextN = (a * n + b) `mod` len
             in case splitAt (n `mod` len) l of
                ([], [])   -> acc
                (x:l1, []) -> loop nextN (x : acc) l1
                (l1, x:l2) -> loop nextN (x : acc) (l1 ++ l2)
        a = 4219421
        b = 1246
        len = length l

sqlQueriesWork :: HashAlgorithm h => h -> SqlFile -> Int -> IO ()
sqlQueriesWork hashAlg (SqlFile f) shuffle = do
    conn <- localMetaCreate f
    let dgs = arbitraryDigest hashAlg
    forM_ dgs $ \dg -> do
        dbAddData conn dg (DataInfo 1 Nothing Nothing Nothing)
    forM_ arbitraryTag $ \t -> do
        dbCreateTag conn t
    dbCommit conn

    let ass = associateTags shuffle dgs arbitraryTag

    forM_ ass $ \(d, ts) -> do
        mapM_ (dbAddTag conn d) ts
    dbCommit conn

    forM_ arbitraryTag $ \tag -> do
        found <- dbTagGetDigests conn tag
        let expected = map fst $ filter (\(_,ts) -> tag `elem` ts) ass
        listCompare ("digest for " ++ tag ++ " missing ") expected found

    forM_ dgs $ \dg -> do
        found <- dbDigestGetTags conn dg
        let expected = maybe [] id $ lookup dg ass
        listCompare ("tags for digest " ++ show dg ++ " missing") expected found

    notTagged <- dbFindDigestsNotTagged conn
    let expected = map fst $ filter (\(_,ts) -> null ts) ass
    listCompare ("not tagged missing") expected notTagged

    return ()
  where
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
