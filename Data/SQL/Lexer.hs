-- | Data.SQL.Lexer
module Data.SQL.Lexer
    ( atomize
    ) where

import Data.Char

data Atom =
      AtomInt        String
    | AtomFloat      String
    | AtomComma
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
    | x == '"'     = eatString [] xs
    | x == '('     = AtomLParen : atomize xs
    | x == ')'     = AtomRParen : atomize xs
    | isOperator x = eatConstruct list AtomOperator isOperator
    | isAlpha x    = eatConstruct list AtomSymbol isAlphaNum
    | otherwise    = AtomError x : atomize xs
  where
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
