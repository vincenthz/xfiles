{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Main (main) where

import           Control.Applicative
import           Control.Monad

import           Test.Tasty
import           Test.Tasty.QuickCheck hiding (shuffle)
import           Test.QuickCheck.Monadic

import           Storage.HashFS.Meta
import           System.Directory
import           Crypto.Hash
import           Data.List

import           Data.Hourglass

import qualified Data.ByteString.Char8 as BC

import           HashfsState

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

withParameters :: (forall h . HashAlgorithm h => SqlFile -> State h -> IO ()) -> PropertyM IO ()
withParameters f = do
    --(SqlHash h, sqlfile@(SqlFile {}), sqlSt) <- pick $ (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    (SqlHash h, sqlfile@(SqlFile {})) <- pick $ (,) <$> arbitrary <*> arbitrary
    sqlSt <- pick (newState h)
    run $ do
        createDirectoryIfMissing True testDirPrefix
        f sqlfile sqlSt
    return ()

--sqlQueriesWork :: HashAlgorithm h => h -> SqlFile -> State h -> IO ()
sqlQueriesWork :: HashAlgorithm h => SqlFile -> State h -> IO ()
sqlQueriesWork (SqlFile f) st = do
    conn <- either error id <$> metaConnect "sqlite3" f

    -- create data in the database
    forEachDigestsAndData st $ \(dg, (di, _)) -> metaAddData conn dg di
    forEachTags st $ \t -> metaCreateTag conn t
    metaCommit conn

    -- tag data
    forEachDigestsAndData st $ \(dg, (_, ts)) -> mapM_ (metaTag conn dg) ts
    metaCommit conn

    -- check that for each tag, we find the right set of data
    forEachTags st $ \tag -> do
        found <- metaTagGetDigests conn tag
        let expected = stateGetDigestTaggedWith st tag 
        listCompare ("digest for " ++ show tag ++ " missing ") expected found

    -- check that for each digests, we find the right set of tags
    forEachDigests st $ \dg -> do
        found <- metaDigestGetTags conn dg
        let expected = stateGetTagOf st dg
        listCompare ("tags for digest " ++ show dg ++ " missing") expected found

    notTagged <- metaFindDigestsNotTagged conn
    let expected = stateGetUntagged st
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
