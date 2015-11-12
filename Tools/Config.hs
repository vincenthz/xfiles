-- |
-- Module      : Tools.Config
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
-- config related types and methods.
--
{-# LANGUAGE OverloadedStrings #-}
module Tools.Config
    ( Config(..)
    , Section(..)
    -- * reading methods
    , readConfigPath
    -- * methods
    , listSections
    , get
    , getAll
    ) where

import           Control.Monad
import           Data.Monoid hiding (getAll)
import           Data.Maybe (catMaybes)
import           System.FilePath
import           Data.List (find, isPrefixOf)
import qualified Data.Set as S

newtype Config = Config [Section]
    deriving (Show,Eq)

data Section = Section
    { sectionName :: String
    , sectionKVs  :: [(String, String)]
    } deriving (Show,Eq)

parseConfig :: String -> Config
parseConfig = Config . reverse . toSections . foldl accSections ([], Nothing) . filter isNotCommentLine . lines
  where toSections (l,Nothing) = l
        toSections (l,Just s)  = s : l

        isNotCommentLine = not . isPrefixOf "#"

        -- a new section in the config file
        accSections (sections, mcurrent) ('[':sectNameE)
            | last sectNameE == ']' =
                let sectName = take (length sectNameE - 1) sectNameE
                 in case mcurrent of
                    Nothing      -> (sections, Just $ Section sectName [])
                    Just current -> (sectionFinalize current : sections, Just $ Section sectName [])
            | otherwise             =
                (sections, mcurrent)
        -- a normal line without having any section defined yet
        accSections acc@(_, Nothing) _ = acc
        -- potentially a k-v line in an existing section
        accSections (sections, Just current) kvLine =
            case break (== '=') kvLine of
                (k,'=':v) -> (sections, Just $ sectionAppend current (strip k, strip v))
                (_,_)     -> (sections, Just current) -- not a k = v line
        -- append a key-value
        sectionAppend (Section n l) kv = Section n (kv:l)
        sectionFinalize (Section n l) = Section n $ reverse l

        strip s = dropSpaces $ reverse $ dropSpaces $ reverse s
          where dropSpaces = dropWhile (\c -> c == ' ' || c == '\t')

readConfigPath :: FilePath -> IO Config
readConfigPath filepath = parseConfig <$> readFile filepath

listSections :: [Config] -> [String]
listSections = S.toList . foldr accSections S.empty
  where accSections (Config sections) set = foldr S.insert set (map sectionName sections)

-- | Get a configuration element in a stack of config file, starting from the top.
get :: [Config] -- ^ stack of config
    -> String   -- ^ section name
    -> String   -- ^ key name
    -> Maybe String
get []            _       _   = Nothing
get (Config c:cs) section key = findOne `mplus` get cs section key
  where findOne = find (\s -> sectionName s == section) c >>= lookup key . sectionKVs

-- | Get many configuration elements in a stack of config file, starting from the top.
getAll :: [Config] -- ^ stack of config
       -> String   -- ^ section name
       -> String   -- ^ key name
       -> [String]
getAll configs section key = mconcat $ catMaybes (map findAll configs)
  where
    findAll :: Config -> Maybe [String]
    findAll (Config c) = map snd . filter ((== key) . fst) . sectionKVs
                     <$> find (\s -> sectionName s == section) c
