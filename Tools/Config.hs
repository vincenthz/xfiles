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
    , KeyValues
    -- * IO methods
    , readConfigPath
    , writeConfigPath
    -- * Config querying
    , listSections
    , get
    , getAll
    , getAllSections
    -- * KeyValues querying
    , kvsGet
    , kvsGetAll
    , kvsFromList
    ) where

import           Control.Monad
import           Data.Monoid hiding (getAll)
import           Data.Maybe (catMaybes)
import           System.FilePath
import           Data.List (find, isPrefixOf)
import qualified Data.Set as S

type Key = String
type Value = String
type SectionName = String

newtype Config = Config { unConfig :: [Section] }
    deriving (Show,Eq)

newtype KeyValues = KeyValues { unKeyValues :: [(Key, Value)] }
    deriving (Show,Eq)

data Section = Section
    { sectionName :: String
    , sectionKVs  :: KeyValues
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
                    Nothing      -> (sections, Just $ Section sectName (KeyValues []))
                    Just current -> (sectionFinalize current : sections, Just $ Section sectName (KeyValues []))
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
        sectionAppend (Section n (KeyValues l)) kv = Section n $ KeyValues (kv:l)
        sectionFinalize (Section n (KeyValues l)) = Section n $ KeyValues $ reverse l

        strip s = dropSpaces $ reverse $ dropSpaces $ reverse s
          where dropSpaces = dropWhile (\c -> c == ' ' || c == '\t')

readConfigPath :: FilePath -> IO Config
readConfigPath filepath = parseConfig <$> readFile filepath

writeConfigPath :: FilePath -> Config -> IO ()
writeConfigPath filepath config = writeFile filepath (unlines $ concatMap serializeSection $ unConfig config)
  where
    serializeSection :: Section -> [String]
    serializeSection section =
        ("[" ++ sectionName section ++ "]") : (map serializeLine (unKeyValues $ sectionKVs section) ++ [""])
    serializeLine :: (Key, Value) -> String
    serializeLine (key, val) =
        key ++ " = " ++ val

listSections :: [Config] -> [String]
listSections = S.toList . foldr accSections S.empty
  where accSections (Config sections) set = foldr S.insert set (map sectionName sections)

-- | Get a configuration element in a stack of config file, starting from the top.
get :: [Config]    -- ^ stack of config
    -> SectionName -- ^ section name
    -> Key         -- ^ key name
    -> Maybe Value
get []            _       _   = Nothing
get (Config c:cs) section key = findOne `mplus` get cs section key
  where findOne = find (\s -> sectionName s == section) c >>= lookup key . unKeyValues . sectionKVs

-- | Get many configuration elements in a stack of config file, starting from the top.
getAll :: [Config]    -- ^ stack of config
       -> SectionName -- ^ section name
       -> Key         -- ^ key name
       -> [Value]
getAll configs section key = mconcat $ catMaybes (map findAll configs)
  where
    findAll :: Config -> Maybe [Value]
    findAll (Config c) = map snd . filter ((== key) . fst) . unKeyValues . sectionKVs
                     <$> find (\s -> sectionName s == section) c

-- | Return all sections matching a specific name
getAllSections :: [Config]
               -> SectionName -- ^ name of the section to match
               -> [KeyValues]
getAllSections configs section =
    map sectionKVs $ mconcat $ map (filter ((==) section . sectionName) . unConfig) configs

-- | Try to find a key in a KeyValue dictionary
kvsGet :: KeyValues
       -> Key
       -> Maybe Value
kvsGet (KeyValues kvs) key = lookup key kvs

-- | Try to find all specific key in a KeyValue dictionary
kvsGetAll :: KeyValues
          -> Key
          -> [Value]
kvsGetAll (KeyValues kvs) key = map snd . filter ((==) key . fst) $ kvs

kvsFromList :: [(Key, Value)] -> KeyValues
kvsFromList = KeyValues
