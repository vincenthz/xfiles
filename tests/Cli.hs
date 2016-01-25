module Main (main) where

import           Control.Applicative
import           Control.Monad

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Monadic

import           Storage.HashFS.Meta
import           System.Directory
import           Data.List

import           Console.Options hiding (defaultMain)

main = defaultMain $ testGroup "main"
    []
