module Console.Options.Flags
    ( parseFlags
    , FlagDesc(..)
    , FlagFragments(..)
    , Flag
    , FlagArgValidation(..)
    , FlagArgDesc(..)
    , FlagError(..)
    -- * fragments
    , FlagFrag(FlagShort, FlagLong, FlagDescription)
    , flattenFragments
    ) where

import Control.Applicative
import Control.Monad
import Console.Options.Nid
import Data.List
import Data.Monoid

data FlagArgValidation = FlagArgValid | FlagArgInvalid String

-- | How to parse a specific flag
data FlagDesc = FlagDesc
    { flagFragments     :: FlagFragments
    , flagNid           :: Nid                    -- ^ flag number. internal value
    , flagArg           :: FlagArgDesc            -- ^ parser for the argument to an flag
    , flagArgValidate   :: String -> FlagArgValidation -- ^ if the argument doesn't validate, return the error message associated, otherwise Nothing
    , flagArity         :: Int
    }

data FlagFrag =
      FlagShort       Char
    | FlagLong        String
    | FlagDescription String
    -- | FlagDefault     String
    | FlagMany        [FlagFrag]
    deriving (Show,Eq)

data FlagFragments = FlagFragments
    { flagShort         :: Maybe Char             -- ^ short flag parser 'o'
    , flagLong          :: Maybe String           -- ^ long flag "flag"
    , flagDescription   :: Maybe String           -- ^ Description of this "flag"
    --, flagDefault       :: Maybe String           -- ^ Has a default
    }

flattenFragments :: FlagFrag -> FlagFragments
flattenFragments frags =
    foldl' flat startVal $ case frags of
                                FlagMany l -> l
                                _          -> [frags]
  where
    startVal = FlagFragments Nothing Nothing Nothing
    flat ff  (FlagShort f)       = ff { flagShort = Just f }
    flat ff  (FlagLong f)        = ff { flagLong = Just f }
    flat ff  (FlagDescription f) = ff { flagDescription = Just f }
    flat acc (FlagMany l)        = foldl' flat acc l

instance Monoid FlagFrag where
    mempty                              = FlagMany []
    mappend (FlagMany l1) (FlagMany l2) = FlagMany (l1 ++ l2)
    mappend (FlagMany l1) o             = FlagMany (l1 ++ [o])
    mappend o             (FlagMany l2) = FlagMany (o : l2)
    mappend o1            o2            = FlagMany [o1,o2]

-- | Whether a flag has an argument, an optional one or always an argument
data FlagArgDesc =
      FlagArgNone
    | FlagArgMaybe
    | FlagArgHave
    deriving (Show,Eq)

data Matching a = NoMatching | Matching a | MatchingWithArg a String

-- | the state of parsing the command line arguments
data ParseState a = ParseState [Flag]      -- Args : in reverse order
                               [String]    -- Unparsed: in reverse order
                               [FlagError] -- errors: in reverse order

type Flag = (Nid, Maybe String)

data FlagError = FlagError FlagDesc Int String

parseFlags :: [FlagDesc]
           -> [String]
           -> ([Flag], [String], [FlagError])
parseFlags flagParsers = loop (ParseState [] [] []) [1..]
  where
        loop :: ParseState a -> [Int] -> [String] -> ([Flag], [String], [FlagError])
        loop _                      []     _      = error "impossible case"
        loop (ParseState os us ers) _      []     = (reverse os, reverse us, reverse ers)
        loop (ParseState os us ers) (i:is) (a:as) =
            case a of
                '-':'-':[]   -> (reverse os, reverse us ++ as, reverse ers)
                '-':'-':long -> loop (processFlag (findLong long)) is as
                '-':short:[] -> loop (processFlag (findShort short)) is as
                _            -> loop (ParseState os (a:us) ers) is as
          where processFlag NoMatching     = ParseState os (a:us) ers
                processFlag (Matching opt) =
                    case flagArg opt of
                        FlagArgNone  -> ParseState ((flagNid opt, Nothing) : os) us ers
                        FlagArgMaybe -> ParseState ((flagNid opt, Nothing) : os) us ers
                        FlagArgHave  ->
                            case as of
                                []     -> let e = mkFlagError opt "required argument missing"
                                           in ParseState os (a:us) (e:ers)
                                (x:_) ->
                                    case (flagArgValidate opt) x of
                                        FlagArgValid          -> ParseState ((flagNid opt, Just x):os) us ers
                                        FlagArgInvalid optErr ->
                                            let e = mkFlagError opt ("invalid argument: " ++ optErr)
                                             in ParseState os us (e:ers)
                processFlag (MatchingWithArg opt arg) =
                    case flagArg opt of
                        FlagArgNone  -> let e = mkFlagError opt "invalid argument, expecting no argument" -- fixme: tell which flag
                                         in ParseState os (a:us) (e:ers)
                        FlagArgMaybe ->
                            case (flagArgValidate opt) arg of
                                FlagArgValid   -> ParseState ((flagNid opt, Just arg):os) us ers
                                FlagArgInvalid optErr ->
                                    let e = mkFlagError opt ("invalid argument: " ++ optErr)
                                     in ParseState os us (e:ers)
                        FlagArgHave  ->
                            case (flagArgValidate opt) arg of
                                FlagArgValid          -> ParseState ((flagNid opt, Just arg):os) us ers
                                FlagArgInvalid optErr ->
                                    let e = mkFlagError opt ("invalid argument: " ++ optErr)
                                     in ParseState os us (e:ers)

                mkFlagError opt s = FlagError opt i s

        findShort short = findRetArg (flagShortMatch short) flagParsers
        findLong long = findRetArg (flagLongMatch long) flagParsers

        flagShortMatch toMatch opt = maybe NoMatching (\x -> if x == toMatch then Matching opt else NoMatching) $ flagShort $ flagFragments opt
        flagLongMatch toMatch opt = maybe NoMatching match $ flagLong $ flagFragments opt
          where match optLong
                    | leftPart == optLong && null rightPart           = Matching opt
                    | leftPart == optLong && isPrefixOf "=" rightPart = MatchingWithArg opt (drop 1 rightPart)
                    | otherwise                                       = NoMatching
                  where (leftPart,rightPart) = splitAt (length optLong) toMatch

        findRetArg _ []         = NoMatching
        findRetArg f (opt:opts) =
            case f opt of
                NoMatching -> findRetArg f opts
                r          -> r
