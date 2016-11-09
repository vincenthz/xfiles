-- | Data.SQL.Lexer
module Data.SQL.Lexer
    ( atomize
    , Atom(..)
    ) where

import Data.Char

data Atom =
      AtomInt        String
    | AtomFloat      String
    | AtomComma
    | AtomDot
    | AtomLParen
    | AtomRParen
    | AtomOperator   String
    | AtomSymbol     String
    | AtomString     String
    | AtomError      Char
    | AtomParseError String
    deriving (Show,Eq)

atomize :: [Char] -> [Atom]
atomize []         = []
atomize list@(x:xs)
    | isDigit x    = eatNum list
    | isSpace x    = atomize xs
    | x == ','     = AtomComma : atomize xs
    | x == '.'     = AtomDot : atomize xs
    | x == '"'     = eatString [] xs
    | x == '\''    = eatSString [] xs
    | x == '('     = AtomLParen : atomize xs
    | x == ')'     = AtomRParen : atomize xs
    | isOperator x = eatConstruct list AtomOperator isOperator
    | isAlpha x    = eatConstruct list AtomSymbol isSymbolChar
    | otherwise    = AtomError x : atomize xs
  where
    isSymbolChar c = isAlphaNum c || c == '_'
    isOperator = flip elem "+-/*=!/&|{}<>~"

    eatNum l =
        let (cs1, cs2) = break (not . isDigit) l
         in case cs2 of
                []    -> AtomInt cs1 : atomize cs2
                '.':f -> let (f1,f2) = break (not . isDigit) f in AtomFloat (cs2 ++ "." ++ f1) : atomize f2
                _     -> AtomInt cs1 : atomize cs2
    eatConstruct l constr f =
        let (cs1, cs2) = break (not . f) l
         in constr cs1 : atomize cs2
    eatString acc []             = AtomParseError ("unterminated string: " ++ show ('"' : reverse acc)) : []
    eatString acc ('"':cs)       = AtomString (reverse acc) : atomize cs
    eatString acc ('\\':'"':cs)  = eatString ('"' : acc) cs
    eatString acc ('\\':'\\':cs) = eatString ('\\': acc) cs
    eatString acc ('\\':cs)      = eatString ('\\': acc) cs
    eatString acc (c:cs)         = eatString (c : acc) cs

    eatSString acc []             = AtomParseError ("unterminated simple string: " ++ show ('"' : reverse acc)) : []
    eatSString acc ('\'':cs)      = AtomString (reverse acc) : atomize cs
    eatSString acc ('\\':'\'':cs) = eatSString ('\'' : acc) cs
    eatSString acc ('\\':'\\':cs) = eatSString ('\\': acc) cs
    eatSString acc ('\\':cs)      = eatSString ('\\': acc) cs
    eatSString acc (c:cs)         = eatSString (c : acc) cs
