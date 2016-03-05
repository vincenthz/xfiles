{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module Main (main) where

import           Control.Applicative
import           Control.Monad

import           Test.Tasty
import           Test.Tasty.QuickCheck hiding (shuffle)
import           Test.QuickCheck.Monadic

import           Storage.HashFS.Meta
import           Storage.HashFS.Query
import           Storage.HashFS
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

    -- find data that are not tagged with anything
    () <- do
        notTagged <- metaFindDigestsNotTagged conn
        let expected = stateGetUntagged st
        listCompare ("not tagged missing") expected notTagged
        return ()

    -- queries
    forM_ (stateGetMultiTags st) $ \(tgs, (l, expected)) ->
        if l <= 1
            then return ()
            else do
                -- for some reason expected is not the right computed data..
                -- recompute it from scratch with the following:
                let expected2 = stateFindMultiTags st tgs
                --listCompare ("expected computed is wrong") expected2 expected
                let qStr = intercalate " && " $ map tagsToQuery tgs
                case parseQuery qStr of
                    Left err -> error ("query " ++ show qStr ++ " invalid: " ++ show err)
                    Right dq -> do
                        got <- metaFindDigestsWhich conn dq
                        putStrLn $ show dq
                        listCompare ("multi tags " ++ show tgs ++ " missing: ") expected2 got
                        return ()
    return ()
  where
    listCompare err expected found
        | sort found /= sort expected = error ("sql: " ++ f ++ "\n" ++ err ++ "\ngot     : " ++ show found ++ "\nexpected: " ++ show expected)
        | otherwise = return ()

    tagsToQuery (Tag (Just Person) x)   = "person == " ++ show x
    tagsToQuery (Tag (Just Location) x) = "location == " ++ show x
    tagsToQuery (Tag (Just Group) x)    = "group == " ++ show x
    tagsToQuery (Tag (Just Other) x)    = "tag == " ++ show x
    tagsToQuery (Tag Nothing x)         = "tag == " ++ show x

sqlTests :: [TestTree]
sqlTests =
    [ testProperty "sql-work" (monadicIO $ withParameters sqlQueriesWork)
    ]

main :: IO ()
main = defaultMain $ testGroup "meta"
    [ testGroup "SQL" sqlTests
    ]
