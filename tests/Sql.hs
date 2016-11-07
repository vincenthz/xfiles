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

    -- multi tag queries
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
                        -- putStrLn $ show dq
                        listCompare ("multi tags " ++ show tgs ++ " missing: ") expected2 got
                        return ()

    -- renaming tags
    -- * renaming tag to something that doesn't exist
    -- * renaming tag to something that exist
    () <- do
        let s1 = head $ stateTags st
            s2 = Tag Group "bliblabloo"
        let expected = stateGetDigestTaggedWith st s1

        _ <- metaRenameTag conn s1 s2
        metaCommit conn

        found <- metaTagGetDigests conn s2
        listCompare ("after renaming to something non existing") expected found

        _ <- metaRenameTag conn s2 s1
        metaCommit conn

        found2 <- metaTagGetDigests conn s1
        listCompare ("after renaming back") expected found2

    when (length (stateTags st) >= 2) $ do
        let s1 = head $ stateTags st
            s2 = head $ drop 1 $ stateTags st
        let expected1  = stateGetDigestTaggedWith st s1
            expected2a = stateGetDigestTaggedWith st s2 -- watch out for duplicates with set of s1
            iexp       = intersect expected1 expected2a
            expected2  = expected2a \\ iexp -- without duplicates

        let expected = sort (expected1 ++ expected2)

        _ <- metaRenameTag conn s1 s2
        metaCommit conn

        found <- metaTagGetDigests conn s2
        listCompare ("after renaming " ++ show s1 ++ " #" ++ show (length expected1) ++ " to existing " ++ show s2 ++ " #" ++ show (length expected2) ++ " intersect=#" ++ show (length iexp))
                expected found

        -- can't rename back without being careful ...

    -- last stuff to do, to cleanup the FS
    metaDisconnect conn >> removeFile f
    return ()
  where
    listCompare err expected found
        | sort found == sort expected = return ()
        | otherwise                   =
            let matching = intersect found expected
                got_exp  = found \\ matching
                exp_got  = expected \\ matching
             in error $ unlines $
                [ "sql: " ++ f
                , err
                , "got#: " ++ show (length found) ++ " found#: " ++ show (length expected) ++ " extra#: " ++ show (length got_exp) ++ " missing#: " ++ show (length exp_got)
                , "extra   : " ++ show got_exp
                , "missing : " ++ show exp_got
                ]
        | otherwise = return ()

    tagsToQuery (Tag Person x)   = "person == " ++ show x
    tagsToQuery (Tag Location x) = "location == " ++ show x
    tagsToQuery (Tag Group x)    = "group == " ++ show x
    tagsToQuery (Tag Other x)    = "tag == " ++ show x

sqlTests :: [TestTree]
sqlTests =
    [ testProperty "sql-work" (monadicIO $ withParameters sqlQueriesWork)
    ]

main :: IO ()
main = defaultMain $ testGroup "meta"
    [ testGroup "SQL" sqlTests
    ]
