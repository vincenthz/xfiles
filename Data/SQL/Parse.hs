{-# LANGUAGE ViewPatterns #-}
module Data.SQL.Parse
    ( parse
    ) where

import Control.Applicative
import Data.SQL.Lexer
import Data.SQL.Types
import Data.Stream
import Data.Char

parse :: String -> Either String (Query, [Atom])
parse = runStream parser . atomize
  where
    parser = do
        p <-     (symbolIs "SELECT" *> pure select)
             <|> (symbolIs "CREATE" *> pure create)
             <|> (symbolIs "INSERT" *> pure insert)
        p
    select = do
        selector <- (operatorIs "*" *> pure SelectorStar)
                <|> (SelectorCols <$> (selCol `sepBy1` isComma))
        symbolIs "FROM"
        table <- tableName
        wh    <- optional whereExpr
        pure $ Select selector table wh
    create = do
        symbolIs "TABLE"
        table <- tableName
        cols  <- parenthesized (columnDecl `sepBy1` isComma)
        pure $ Create table cols
      where
        columnDecl = ColumnDecl <$> columnName <*> columnType <*> columnConstraint
        columnType =
                (toParamType <$> functionCall)
            <|> (toSimpleType <$> eatRet getSymbol)
        columnConstraint = many constr
          where constr = (symbolIs "PRIMARY" *> symbolIs "KEY" *> pure Constraint_PrimaryKey)
                     <|> (symbolIs "FOREIGN" *> symbolIs "KEY" *> pure Constraint_ForeignKey)
                     <|> (symbolIs "UNIQUE" *> pure Constraint_Unique)
                     <|> (symbolIs "NOT" *> symbolIs "NULL" *> pure Constraint_NotNull)
                     <|> (symbolIs "DEFAULT" *> symbolIs "NULL" *> pure Constraint_Default)

        toParamType :: FunctionCall -> ColumnType
        toParamType (FunctionCall (FunctionName (map toUpper -> fname)) params) =
            case fname of
                "VARCHAR" ->
                    case params of
                        [ValueInt n] -> ColumnString $ CStr_VarChar (downSize n)
                        _            -> error ("unknown parameters to types : " ++ fname ++ " " ++ show params)
                "CHAR"    ->
                    case params of
                        [ValueInt n] -> ColumnString $ CStr_Char (downSize n)
                        _            -> error ("unknown parameters to types : " ++ fname ++ " " ++ show params)
                "NUMERIC"    ->
                    case params of
                        [ValueInt n] -> ColumnNumeric $ CNum_Decimal (downSize n) 0
                        [ValueInt n, ValueInt n2] -> ColumnNumeric $ CNum_Decimal (downSize n) (downSize n2)
                        _            -> error ("unknown parameters to types : " ++ fname ++ " " ++ show params)
                _ -> error ("unknown function type: " ++ fname ++ " " ++ show params)

        toSimpleType :: String -> ColumnType
        toSimpleType (map toUpper -> simpleType) =
            case simpleType of
                "SMALLINT" -> ColumnNumeric CNum_Int16
                "INT"      -> ColumnNumeric CNum_Int32
                "BIGINT"   -> ColumnNumeric CNum_Int64
                _          -> error ("unknown simple type: " ++ simpleType)

    insert = do
        symbolIs "INTO"
        table <- tableName
        mcols <- optional (parenthesized (columnName `sepBy1` isComma))
        symbolIs "VALUES"
        vals <- parenthesized (value `sepBy1` isComma)
        pure $ Insert table mcols vals

    selCol = columnUdf <|> (SelectorColName <$> columnName <*> optional (symbolIs "AS" *> columnName))

    whereExpr =
        symbolIs "WHERE" *>
        (WhereExpr <$> (parseAnd <|> parseExpr))
      where
        parseAnd = do
            o1 <- parseOr
            operatorIs "&&"
            o2 <- parseOr
            return (ExprAnd o1 o2)
        parseOr = do
            e1 <- parseExpr
            operatorIs "||"
            e2 <- parseExpr
            return (ExprOr e1 e2)
        parseExpr =
                parenthesized parseAnd
            <|> (ExprBin <$> operand <*> operator <*> operand)
            <|> do c <- columnName
                   symbolIs "LIKE"
                   pat <- eatRet getString
                   return (ExprLike c pat)

        operand = value

        operator = (operatorIs "="  *> pure ExprEQ)
               <|> (operatorIs "<>" *> pure ExprNE)
               <|> (operatorIs ">=" *> pure ExprGE)
               <|> (operatorIs "<=" *> pure ExprLE)
               <|> (operatorIs ">"  *> pure ExprGT)
               <|> (operatorIs "<"  *> pure ExprLT)

    functionCall = FunctionCall <$> functionName
                                <*> parenthesized (value `sepBy` isComma)

    columnUdf = SelectorColUdf <$> functionCall

downSize :: Integer -> Int
downSize n
    | n > toInteger (maxBound :: Int) = error "cannot transform integer to int: out of bounds"
    | otherwise                       = fromIntegral n

value :: Stream Atom Value
value = eatRet $ \a ->
    case a of
        AtomString s -> Just $ ValueString s
        AtomInt s    -> Just $ ValueInt (read s)
        AtomSymbol s -> Just $ ValueVar s
        _            -> Nothing

eqCI :: String -> String -> Bool
eqCI a b = map toLower a == map toLower b

eatAtom :: (Show a, Eq a) => a -> Stream a ()
eatAtom a = eat ((==) a)

functionName :: Stream Atom FunctionName
functionName = FunctionName <$> eatRet getSymbol

tableName :: Stream Atom TableName
tableName = TableName <$> eatRet getSymbol

columnName :: Stream Atom ColumnName
columnName = ColumnName <$> eatRet getSymbol

symbolIs :: String -> Stream Atom ()
symbolIs s = eat (maybe False (eqCI s) . getSymbol)
operatorIs :: String -> Stream Atom ()
operatorIs s = eat (maybe False (eqCI   s) . getOperator)

getSymbol :: Atom -> Maybe String
getSymbol (AtomSymbol a) = Just a
getSymbol _              = Nothing

getOperator :: Atom -> Maybe String
getOperator (AtomOperator a) = Just a
getOperator _                = Nothing

getString :: Atom -> Maybe String
getString (AtomString s) = Just s
getString _              = Nothing

parenthesized :: Stream Atom a -> Stream Atom a
parenthesized = between (eatAtom AtomLParen) (eatAtom AtomRParen)

isComma :: Stream Atom ()
isComma = eat (== AtomComma)

-- generic combinator stuff

between :: Applicative f => f op -> f cl -> f a -> f a
between popen pclose p = popen *> p <* pclose

sepBy1 :: Alternative f => f a -> f sep -> f [a]
sepBy1 p sep = scan where scan = liftA2 (:) p ((sep *> scan) <|> pure [])

sepBy :: Alternative f => f a -> f sep -> f [a]
sepBy p s = liftA2 (:) p ((s *> sepBy1 p s) <|> pure []) <|> pure []
