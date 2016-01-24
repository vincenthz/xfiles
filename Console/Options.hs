{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
module Console.Options
    (
    -- * Running
      defaultMain
    , defaultMainWith
    -- * Description
    , programName
    , programVersion
    , programDescription
    , command
    , flag
    , FlagFrag(..)
    , flagArg
    , conflict
    , argument
    , remainingArguments
    , action
    , description
    , Action
    -- * Arguments
    , FlagParser(..)
    , Flag
    , Arg
    ) where

import           Console.Options.Flags hiding (Flag, flagArg)
import qualified Console.Options.Flags as F
import           Console.Options.Nid
import           Console.Options.Utils
import           Console.Options.Monad
import           Console.Options.Types
import           Console.Display (justify, Justify(..))

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Writer

import           Data.List
import           Data.Version

import           System.Environment (getArgs, getProgName)
import           System.Exit

----------------------------------------------------------------------
setDescription :: String -> Command -> Command
setDescription desc  (Command hier _ opts act)    = Command hier desc opts act

setAction :: Action -> Command -> Command
setAction ioAct (Command hier desc opts _)   = Command hier desc opts ioAct

addOption :: FlagDesc -> Command -> Command
addOption opt   (Command hier desc opts act) = Command hier desc (opt : opts) act

addArg :: Argument -> Command -> Command
addArg arg = modifyHier $ \hier ->
    case hier of
        CommandLeaf l  -> CommandLeaf (arg:l)
        CommandTree {} -> hier -- ignore argument in a hierarchy.
----------------------------------------------------------------------

data FlagParser a =
      FlagRequired (ValueParser a)
    | FlagOptional a (ValueParser a)

type ValueParser a = String -> Either String a

-- use something with faster lookup. using list for now, to not bring dep
newtype Flags = Flags [(Nid, Maybe String)] -- @ flag arguments

data Args = Args [String]              -- @ unnamed pinned arguments
                 [String]              -- @ remaining unnamed arguments

defaultMain :: OptionDesc () -> IO ()
defaultMain dsl = getArgs >>= defaultMainWith dsl

defaultMainWith :: OptionDesc () -> [String] -> IO ()
defaultMainWith dsl args = do
    let descState = gatherDesc dsl
    runOptions (stMeta descState) (stCT descState) args

--helpSubcommand :: [String] -> IO ()

help :: ProgramMeta -> Command -> IO ()
help pmeta (Command hier _ commandOpts _) = mapM_ putStrLn . lines $ snd $ runWriter $ do
    tell (maybe "<program>" id (programMetaName pmeta) ++ " version " ++ maybe "<undefined>" id (programMetaVersion pmeta) ++ "\n")
    tell "\n"
    maybe (return ()) (\d -> tell d >> tell "\n\n") (programMetaDescription pmeta)
    tell "Options:\n"
    tell "\n"
    mapM_ (tell . printOpt 0) commandOpts
    case hier of
        CommandTree subs -> do
            tell "\n"
            tell "Commands:\n"
            let cmdLength = maximum (map (length . fst) subs) + 2
            mapM_ (\(n, c) -> tell $ indent 2 (justify JustifyRight cmdLength n ++ getCommandDescription c ++ "\n")) subs
            tell "\n"
            mapM_ (printSub 0) subs
        CommandLeaf _    ->
            return ()
  where
    printSub iLevel (name, cmdOpt) = do
        tell ("\n" ++ name ++ " options:\n\n")
        mapM_ (tell . printOpt iLevel) (getCommandOptions cmdOpt)
        case getCommandHier cmdOpt of
            CommandTree _ -> do
                return ()
            CommandLeaf _ -> do
                return ()
        --tell . indent 2 ""

    printOpt iLevel fd =
        let optShort = maybe (replicate 2 ' ') (\c -> "-" ++ [c]) $ flagShort ff
            optLong  = maybe (replicate 8 ' ') (\s -> "--" ++ s) $ flagLong ff
            optDesc  = maybe "" ("  " ++) $ flagDescription ff
         in indent (iLevel + 2) $ intercalate " " [optShort, optLong, optDesc] ++ "\n"
      where
        ff = flagFragments fd

runOptions :: ProgramMeta
           -> Command  -- commands
           -> [String] -- arguments
           -> IO ()
runOptions pmeta ct allArgs
    | "--help" `elem` allArgs = help pmeta ct
    | "-h" `elem` allArgs     = help pmeta ct
    | otherwise               = go [] ct allArgs
  where
        -- parse recursively using a Command structure
        go :: [[F.Flag]] -> Command -> [String] -> IO ()
        go parsedOpts (Command hier _ commandOpts act) unparsedArgs =
            case parseFlags commandOpts unparsedArgs of
                (opts, unparsed, [])  -> do
                    case hier of
                        -- if we have sub commands, then we pass the unparsed options
                        -- to their parsers
                        CommandTree subs -> do
                            case unparsed of
                                []     -> errorExpectingMode subs
                                (x:xs) -> case lookup x subs of
                                                Nothing      -> errorInvalidMode x subs
                                                Just subTree -> go (opts:parsedOpts) subTree xs
                        -- no more subcommand (or none to start with)
                        CommandLeaf unnamedArgs ->
                            case validateUnnamedArgs (reverse unnamedArgs) unparsed of
                                Left err   -> errorUnnamedArgument err
                                Right args -> do
                                    let flags = Flags $ concat (opts:parsedOpts)
                                    act (getFlag flags) (getArg args)
                (_, _, ers) -> do
                    mapM_ showOptionError ers
                    exitFailure

        validateUnnamedArgs :: [Argument] -> [String] -> Either String Args
        validateUnnamedArgs argOpts l =
            v [] argOpts >>= \(opts, _hasCatchall) -> do
                let unnamedRequired = length opts
                if length l < unnamedRequired
                    then Left "missing arguments"
                    else Right $ uncurry Args $ splitAt unnamedRequired l
          where
            v :: [Argument] -> [Argument] -> Either String ([Argument], Bool)
            v acc []                    = Right (reverse acc, False)
            v acc (a@(Argument {}):as)  = v (a:acc) as
            v acc ((ArgumentCatchAll {}):[]) = Right (reverse acc, True)
            v _   ((ArgumentCatchAll {}):_ ) = Left "arguments expected after remainingArguments"

        showOptionError (FlagError opt i s) = do
            let optName = (maybe "" (:[]) $ flagShort $ flagFragments opt) ++ " " ++ (maybe "" id $ flagLong $ flagFragments opt)
            hPutErrLn ("error: " ++ show i ++ " option " ++ optName ++ " : " ++ s)

        errorUnnamedArgument err = do
            mapM_ hPutErrLn $
                [ "error: " ++ err
                , ""
                ]
            exitFailure

        errorExpectingMode subs = do
            mapM_ hPutErrLn $
                [ "error: expecting one of the following mode: "
                , ""
                ] ++ map (indent 4 . fst) subs
            exitFailure
        errorInvalidMode got subs = do
            mapM_ hPutErrLn $
                [ "error: invalid mode '" ++ got ++ "', expecting one of the following mode: "
                , ""
                ] ++ map (indent 4 . fst) subs
            exitFailure

indent :: Int -> String -> String
indent n s = replicate n ' ' ++ s

-- | Set the program name
programName :: String -> OptionDesc ()
programName s = modify $ \st -> st { stMeta = (stMeta st) { programMetaName = Just s } }

-- | Set the program version
programVersion :: Version -> OptionDesc ()
programVersion s = modify $ \st -> st { stMeta = (stMeta st) { programMetaVersion = Just $ showVersion s } }

-- | Set the program description
programDescription :: String -> OptionDesc ()
programDescription s = modify $ \st -> st { stMeta = (stMeta st) { programMetaDescription = Just s } }

-- | Set the description for a command
description :: String -> OptionDesc ()
description doc = modify $ \st -> st { stCT = setDescription doc (stCT st) }

modifyHier :: (CommandHier -> CommandHier) -> Command -> Command
modifyHier f (Command hier desc opts act) = Command (f hier) desc opts act

modifyCT :: (Command -> Command) -> OptionDesc ()
modifyCT f = modify $ \st -> st { stCT = f (stCT st) }

-- | Create a new sub command
command :: String -> OptionDesc () -> OptionDesc ()
command name sub = do
    let subSt = gatherDesc sub
    modifyCT (addCommand (stCT subSt))
    --modify $ \st -> st { stCT = addCommand (stCT subSt) $ stCT st }
  where addCommand subTree = modifyHier $ \hier ->
            case hier of
                CommandLeaf _ -> CommandTree [(name,subTree)]
                CommandTree t -> CommandTree ((name, subTree) : t)

-- | Set the action to run in this command
action :: Action -> OptionDesc ()
action ioAct = modify $ \st -> st { stCT = setAction ioAct (stCT st) }

-- | Flag option either of the form -short or --long
--
-- for flag that doesn't have parameter, use 'flag'
flagArg :: FlagFrag -> FlagParser a -> OptionDesc (Flag a)
flagArg frag fp = do
    nid <- getNextID

    let fragmentFlatten = flattenFragments frag

    let opt = FlagDesc
                { flagFragments   = fragmentFlatten
                , flagNid         = nid
                , F.flagArg       = argp
                , flagArgValidate = validator
                }

    modify $ \st -> st { stCT = addOption opt (stCT st) }

    case mopt of
        Just a  -> return (FlagParamOpt nid a parser)
        Nothing -> return (FlagParam nid parser)
  where
    (argp, parser, mopt, validator) = case fp of
        FlagRequired p   -> (FlagArgHave, toArg p, Nothing, isValid p)
        FlagOptional a p -> (FlagArgMaybe, toArg p, Just a, isValid p)

    toArg :: (String -> Either String a) -> String -> a
    toArg p = either (error "internal error toArg") id . p

    isValid f = either FlagArgInvalid (const FlagArgValid) . f

-- | Flag option either of the form -short or --long
--
-- for flag that expect a value (optional or mandatory), uses 'flagArg'
flag :: FlagFrag -> OptionDesc (Flag Bool)
flag frag = do
    nid <- getNextID

    let fragmentFlatten = flattenFragments frag

    let opt = FlagDesc
                { flagFragments   = fragmentFlatten
                , flagNid         = nid
                , F.flagArg       = FlagArgNone
                , flagArgValidate = error ""
                }

    modify $ \st -> st { stCT = addOption opt (stCT st) }
    return (FlagNoParam nid)

-- | An unnamed argument
--
-- For now, argument in a point of tree that contains sub trees will be ignored.
-- TODO: record a warning or add a strict mode (for developping the CLI) and error.
argument :: String -> ValueParser a -> OptionDesc (Arg a)
argument name fp = do
    idx <- getNextIndex
    let a = Argument { argumentName        = name
                     , argumentDescription = ""
                     , argumentValidate    = either Just (const Nothing) . fp
                     }
    modifyCT $ addArg a
    return (Arg idx (either (error "internal error") id . fp))

remainingArguments :: String -> OptionDesc (Arg [String])
remainingArguments name = do
    let a = ArgumentCatchAll { argumentName        = name
                             , argumentDescription = ""
                             }
    modifyCT $ addArg a
    return ArgsRemaining

-- | give the ability to set options that are conflicting with each other
-- if option a is given with option b then an conflicting error happens
conflict :: Flag a -> Flag b -> OptionDesc ()
conflict = undefined

getFlag :: Flags -> (forall a . Flag a -> Maybe a)
getFlag (Flags flagArgs) (FlagParam nid p) =
    case lookup nid flagArgs of
        Just Nothing      -> error "internal error: parameter is missing" -- something is wrong with the flag parser
        Just (Just param) -> Just (p param)
        Nothing           -> Nothing
getFlag (Flags flagArgs) (FlagParamOpt nid a p) =
    case lookup nid flagArgs of
        Just (Just param) -> Just (p param)
        Just Nothing      -> Just a
        Nothing           -> Nothing
getFlag (Flags flagArgs) (FlagNoParam nid) =
    fmap (const True) $ lookup nid flagArgs

getArg :: Args -> (forall a . Arg a -> a)
getArg (Args unnamedArgs _) (Arg index p) =
    p (unnamedArgs !! index)
getArg (Args _ otherArgs) ArgsRemaining =
    otherArgs
