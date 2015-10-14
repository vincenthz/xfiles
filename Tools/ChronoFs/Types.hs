module Tools.ChronoFs.Types where

import Data.Default.Class
import System.Posix.Types
import Data.Time.Clock.POSIX
import Data.ByteString (ByteString)
import Filesystem.Path
import Filesystem.Path.CurrentOS ()
import Prelude hiding (FilePath)
import Data.Word
import Crypto.Hash

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
    , entHash  :: !EntContent
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

data EntContent = ContentLink ByteString | ContentHash Hash
    deriving (Show,Eq)

newtype Hash = Hash (Digest SHA512)
    deriving (Show,Eq)

instance Default Stats where
    def = Stats 0 0 0 0 0