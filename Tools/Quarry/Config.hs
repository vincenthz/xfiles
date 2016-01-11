module Tools.Quarry.Config
    ( QuarryDB
    , QuarryConfig(..)
    ) where

import Database.HDBC.Sqlite3 (Connection)
import Tools.Quarry.Types
import Tools.Quarry.DB.Types
import Tools.Quarry.Cache
import Storage.HashFS (Provider)
import Crypto.Hash (HashAlgorithm)

-- | Config
data QuarryConfig h = QuarryConfig
    { connection :: Connection
    , hash       :: h
    , providers  :: [Provider h]
    , cacheTags  :: CacheTable KeyTag Tag
    }

type QuarryDB = Connection
