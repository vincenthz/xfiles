-- | Data.SQL.Lexer
module Data.SQL.Lexer
    ( atomize
    , Atom(..)
    ) where

import Data.Char
import Data.List (isPrefixOf)
import Data.ByteString (ByteString, pack)

data Atom =
      AtomInt        String
    | AtomFloat      String
    | AtomComma
    | AtomSemiColon
    | AtomDot
    | AtomLParen
    | AtomRParen
    | AtomOperator   String
    | AtomSymbol     String
    | AtomString     String
    | AtomBytea      ByteString
    | AtomError      Char
    | AtomParseError String
    deriving (Show,Eq)

atomize :: [Char] -> [Atom]
atomize []         = []
atomize list@(x:xs)
    | isDigit x    = eatNum list
    | isSpace x    = atomize xs
    | isPrefixOf "--" list = comment list
    | x == ';'     = AtomSemiColon : atomize xs
    | x == ','     = AtomComma : atomize xs
    | x == '.'     = AtomDot : atomize xs
    | x == '"'     = eatString [] xs
    | x == '\''    = eatSString [] xs
    | x == '('     = AtomLParen : atomize xs
    | x == ')'     = AtomRParen : atomize xs
    | isPrefixOf byteAStart list = eatByteA (drop (length byteAStart) list)
    | isOperator x = eatConstruct list AtomOperator isOperator
    | isAlpha x    = eatConstruct list AtomSymbol isSymbolChar
    | otherwise    = AtomError x : atomize xs
  where
    byteAStart = "E'\\x"
    isSymbolChar c = isAlphaNum c || c == '_'
    isOperator = flip elem "+-/*=!/&|{}<>~"

    comment l = let l2 = dropWhile (/= '\n') l in atomize (drop 1 l2)

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

    eatByteA l =
        let (bEsc, bRem) = span (\c -> isHexDigit c || c == ' ') l
         in case bRem of
                []      -> AtomParseError ("unterminated bytearray: end of file") : []
                '\'':cs -> AtomBytea (unhex bEsc) : atomize cs
                c:cs    -> AtomParseError ("unterminated bytearray: " ++ show c ++ " " ++ take 5 cs) : []

    unhex = pack . loop
      where
        loop []      = []
        loop (_:[])  = error "impossible"
        loop (' ':a:b:c) = unhexOne a * 16 + unhexOne b : loop c
        loop (a:b:c) = unhexOne a * 16 + unhexOne b : loop c
        unhexOne c | c >= '0' && c <= '9' = fromIntegral (fromEnum c - fromEnum '0')
                   | c >= 'a' && c <= 'f' = fromIntegral (fromEnum c - fromEnum 'a' + 10)
                   | c >= 'A' && c <= 'F' = fromIntegral (fromEnum c - fromEnum 'A' + 10)
                   | otherwise            = error "invalid character"


    eatSString acc []             = AtomParseError ("unterminated simple string: " ++ show ('"' : reverse acc)) : []
    eatSString acc ('\'':cs)      = AtomString (reverse acc) : atomize cs
    eatSString acc ('\\':'\'':cs) = eatSString ('\'' : acc) cs
    eatSString acc ('\\':'\\':cs) = eatSString ('\\': acc) cs
    eatSString acc ('\\':cs)      = eatSString ('\\': acc) cs
    eatSString acc (c:cs)         = eatSString (c : acc) cs
