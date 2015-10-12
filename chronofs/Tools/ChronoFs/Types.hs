module Tools.ChronoFs.Types where

import Data.Default
import System.Posix.Types
import Data.Time.Clock.POSIX
import Data.ByteString (ByteString)
import Filesystem.Path
import Filesystem.Path.CurrentOS ()
import Prelude hiding (FilePath)
import Data.Word

data Stats = Stats
    { statsErrors    :: Int
    , statsDups      :: Int
    , statsSkipped   :: Int
    , statsProcessed :: Int
    , statsAddedBytes :: Word64
    } deriving (Show)

data EntType = EntFile | EntDir | EntLink
    deriving (Show,Eq)

data Ent = Ent
    { entType  :: EntType
    , entPerms :: FileMode
    , entMTime :: !POSIXTime
    , entCTime :: !POSIXTime
    , entHash  :: !Hash
    , entName  :: !FilePath
    } deriving (Show,Eq)

data FileType = BlockDevice | CharDevice | NamedPipe | RegularFile | Directory | SymbolicLink | Socket
    deriving (Show,Eq)

data FileMeta = FileMeta
    { fileMetaType    :: FileType
    , fileMetaMode    :: FileMode
    , fileMetaModTime :: POSIXTime
    , fileMetaChTime  :: POSIXTime
    , fileMetaSize    :: Word64
    } deriving (Show,Eq)

newtype Hash = Hash ByteString
    deriving (Show,Eq)

instance Default Stats where
    def = Stats 0 0 0 0 0
