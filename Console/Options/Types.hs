{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
module Console.Options.Types
    ( Argument(..)
    , Command(..)
    , CommandHier(..)
    , Action
    , UnnamedIndex
    -- * User Binders to retrieve their options
    , Flag(..)
    , Arg(..)
    ) where

import           Console.Options.Flags (FlagDesc)
import           Console.Options.Nid

-- | A unnamed argument
data Argument =
      Argument
        { argumentName        :: String
        , argumentDescription :: String
        , argumentValidate    :: String -> Maybe String
        }
    | ArgumentCatchAll
        { argumentName        :: String
        , argumentDescription :: String
        }

data Flag a where
    FlagNoParam      :: Nid -> Flag Bool
    --FlagLevel        :: Nid -> Flag Int
    FlagParamOpt     :: Nid -> a -> (String -> a) -> Flag a
    FlagParam        :: Nid -> (String -> a) -> Flag a

data Arg a where
    Arg           :: UnnamedIndex -> (String -> a) -> Arg a
    ArgsRemaining :: Arg [String]

type UnnamedIndex = Int

-- A command that is composed of a hierarchy
--
data Command r = Command
    { getCommandHier        :: CommandHier r
    , getCommandDescription :: String
    , getCommandOptions     :: [FlagDesc]
    , getCommandAction      :: Maybe (Action r)
    }

-- | Recursive command tree
data CommandHier r =
      CommandTree [(String, Command r)]
    | CommandLeaf [Argument]

-- | Represent a program to run
type Action r = (forall a . Flag a -> Maybe a) -- flags
             -> (forall a . Arg a -> a)        -- unnamed argument
             -> r
