module ShellGlob
    ( Glob
    , mkGlob
    , matchGlob
    ) where

import Text.Regex.Posix ((=~))

data Glob = Glob String String
    deriving (Show,Eq)

globToRegex :: String -> String
globToRegex s = '^' : globToRegex' s ++ "$"
  where globToRegex' :: String -> String
        globToRegex' "" = ""

        globToRegex' ('*':cs) = ".*" ++ globToRegex' cs

        globToRegex' ('?':cs) = '.' : globToRegex' cs

        globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
        globToRegex' ('[':c:cs)     = '['  :  c : charClass cs
        globToRegex' ('[':_)        = error "unterminated character class"

        globToRegex' (c:cs) = escape c ++ globToRegex' cs

        escape :: Char -> String
        escape c | c `elem` regexChars = '\\' : [c]
                 | otherwise = [c]
            where regexChars = "\\+()^$.{}]|"

        charClass :: String -> String
        charClass (']':cs) = ']' : globToRegex' cs
        charClass (c:cs)   = c : charClass cs
        charClass []       = error "unterminated character class"

mkGlob :: String -> Glob
mkGlob s = Glob s (globToRegex s)

matchGlob :: Glob -> String -> Bool
matchGlob (Glob _ g) s = s =~ g
