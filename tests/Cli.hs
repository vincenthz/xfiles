module Main (main) where

import           Control.Applicative
import           Control.Monad

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Monadic

import           Storage.HashFS.Meta
import           System.Directory
import           Data.List
import           Data.Monoid
import           Data.Functor.Identity

import           Console.Options hiding (defaultMain)

commandFoo = command "foo" $ do
    action $ \a _ -> return True

--commandBar :: Monad m => OptionDesc (m Bool) ()
commandBar :: OptionDesc (Identity Bool) ()
commandBar = command "bar" $ do
    a <- flagArg (FlagShort 'a' <> FlagLong "aaa") (FlagRequired Right)
    action $ \a _ -> do
        return True

testParseHelp name f = testProperty name $ runIdentity $ do
    case snd f of
        OptionHelp -> return True
        _          -> return False

testParseSuccess name f =
    testProperty name $ runIdentity $ do
        let (_,r) = f
         in case r of
                OptionSuccess f -> f
                _               -> return False

main = defaultMain $ testGroup "options"
    [ testGroup "help"
        [ testParseHelp "1" $ parseOptions (commandBar) ["options", "argument", "--help", "a"]
        , testParseHelp "2" $ parseOptions (commandBar) ["options", "argument", "-h", "a"]
        ]
    , testGroup "success"
        [ testParseSuccess "1" $ parseOptions (commandBar) ["bar", "-a", "option-a"]
        , testParseSuccess "2" $ parseOptions (commandBar >> commandFoo) ["foo", "a"]
        ]
    ]
