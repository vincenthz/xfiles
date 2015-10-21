{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
module Console.Options.Monad
    ( ProgramDesc(..)
    , OptionDesc
    , gatherDesc
    , getNextID
    , getNextIndex
    ) where

import           Console.Options.Nid
import           Console.Options.Types
import           Console.Options.Utils
import           Control.Monad.State
import           Control.Monad.Identity
import           System.Exit

-- the current state of the program description
-- as the monad unfold ..
data ProgramDesc = ProgramDesc
    { stName        :: Maybe String  -- program name
    , stDescription :: Maybe String  -- program description
    , stCT          :: Command       -- the command
    , stNextID      :: !NidGenerator -- next id for flag
    , stNextIndex   :: !UnnamedIndex -- next index for unnamed argument
    }

newtype OptionDesc a = OptionDesc { runOptionDesc :: StateT ProgramDesc Identity a }
    deriving (Functor,Applicative,Monad,MonadState ProgramDesc)

gatherDesc :: OptionDesc a -> ProgramDesc
gatherDesc dsl = runIdentity $ execStateT (runOptionDesc dsl) initialProgramDesc

initialProgramDesc :: ProgramDesc
initialProgramDesc = ProgramDesc { stName        = Nothing
                                 , stDescription = Nothing
                                 , stCT          = iniCommand
                                 , stNextID      = nidGenerator
                                 , stNextIndex   = 0
                                 }
  where
    iniCommand :: Command
    iniCommand = Command (CommandLeaf []) "no description" [] noAction

    noAction :: (forall a . Flag a -> Maybe a)
             -> (forall a . Arg a -> a)
             -> IO ()
    noAction _ _ = do
        hPutErrLn "error: no action defined, using default handler"
        exitFailure

-- | Return the next unique argument ID
getNextID :: OptionDesc Nid
getNextID = do
    (nid, nidGen) <- nidNext . stNextID <$> get
    modify $ \st -> st { stNextID = nidGen }
    return nid

getNextIndex :: OptionDesc UnnamedIndex
getNextIndex = do
    idx <- stNextIndex <$> get
    modify $ \st -> st { stNextIndex = idx + 1 }
    return idx
