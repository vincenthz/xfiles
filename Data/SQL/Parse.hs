{-# LANGUAGE ViewPatterns #-}
module Data.SQL.Parse
    ( parse
    ) where

import Control.Applicative
import Data.SQL.Lexer
import Data.SQL.Types
import Data.Stream
import Data.Char

parse :: String -> Either String ([Query], [Atom])
parse = runStream queries . atomize
  where
    semi = eat (== AtomSemiColon)
    queries = (parser `sepBy` semi) <* many semi
    parser = do
        p <-     (symbolIs "SELECT" *> pure (Select <$> selectQuery))
             <|> (symbolIs "INSERT" *> pure (Insert <$> insert))
             <|> (symbolIs "UPDATE" *> pure (Update <$> update))
             <|> (symbolIs "CREATE" *> pure (Create <$> create))
             <|> (symbolIs "DROP" *> pure (Drop <$> pDrop))
        p
    selectKW = (symbolIs "SELECT" *> selectQuery)

    selectQuery = do
        sel <- selectorAs `sepBy1` isComma
        symbolIs "FROM"
        sources <- source `sepBy1` isComma
        wh    <- optional whereExpr
        gb    <- optional groupBy
        ob    <- optional orderBy
        pure $ SelectQuery sel sources wh gb ob

    selectorAs = do
        sel <- selectorCol
        as  <- optional (symbolIs "AS" *> columnName)
        pure $ Selector sel as

    selectorCol = do
            (SelectorColStar Nothing <$ operatorIs "*")
        <|> (SelectorColStar . Just <$> (tableName <* isDot <* operatorIs "*"))
        <|> (SelectorColUdf <$> functionName <*> (parenthesized (selectorCol `sepBy1` isComma)))
        <|> (SelectorColName <$> pMQColumnName)

    source = do
        table <- tableName
        alias <- optional tableName
        pure $ SourceTable table alias

    pDrop = do
        symbolIs "TABLE"
        ine <- optional ifExist
        table <- tableName
        pure $ DropTable ine table

    update = do
        table <- tableName
        symbolIs "SET"
        cvals <- cval `sepBy` isComma
        wh    <- optional whereExpr
        pure $ UpdateQuery table cvals wh
      where
        cval = do
            cn <- columnName
            operatorIs "="
            val <- value
            return (cn, val)

    create = do
        symbolIs "TABLE"
        ine <- optional ifNotExist
        table <- tableName
        cols  <- parenthesized (columnDecl `sepBy1` isComma)
        pure $ CreateQuery ine table cols
      where
        columnDecl = ColumnDecl <$> columnName <*> columnType <*> columnConstraint
        columnType =
                (toParamType <$> functionCall)
            <|> (toSimpleType <$> eatRet getSymbol)
        columnConstraint = many constr
          where constr = (symbolIs "PRIMARY" *> symbolIs "KEY" *> pure Constraint_PrimaryKey)
                     <|> (symbolIs "FOREIGN" *> symbolIs "KEY" *> (Constraint_References <$> qColumnName))
                     <|> (symbolIs "REFERENCES" *> (Constraint_References <$> (QualifiedColumnName <$> tableName <*> parenthesized columnName)))
                     <|> (symbolIs "UNIQUE" *> pure Constraint_Unique)
                     <|> (symbolIs "NOT" *> symbolIs "NULL" *> pure Constraint_NotNull)
                     <|> (symbolIs "DEFAULT" *> symbolIs "NULL" *> pure Constraint_Default)
                     <|> constraintFunction
                     <|> constraintOther
                constraintFunction = do
                    s <- eatRet getSymbol
                    params <- parenthesized (eatRet getSymbol `sepBy1` isComma)
                    return $ Constraint_UnknownFunction s params
                constraintOther =
                    Constraint_Unknown <$> eatRet getSymbol

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
                _ -> ColumnFunctionUnknown fname params

        toSimpleType :: String -> ColumnType
        toSimpleType (map toUpper -> simpleType) =
            case simpleType of
                "SMALLINT" -> ColumnNumeric CNum_Int16
                "INT"      -> ColumnNumeric CNum_Int32
                "BIGINT"   -> ColumnNumeric CNum_Int64
                _          -> ColumnUnknown simpleType

    insert = do
        symbolIs "INTO"
        table <- tableName
        mcols <- optional (parenthesized (columnName `sepBy1` isComma))
        symbolIs "VALUES"
        vals <- parenthesized (value `sepBy1` isComma)
        pure $ InsertQuery table mcols vals

    ifNotExist = symbolIs "IF" *> symbolIs "NOT" *> symbolIs "EXISTS" *> pure IfNotExist
    ifExist = symbolIs "IF" *> symbolIs "EXISTS" *> pure IfExist

    whereExpr =
        symbolIs "WHERE" *>
        (WhereExpr <$> (parseAnd <|> parseExpr))
      where
        parseAnd = do
            o1 <- parseOr
            os <- many (symbolIs "AND" *> parseOr)
            pure $ foldl ExprAnd o1 os
        parseOr = do
            e1 <- parseNotExpr
            es <- many (symbolIs "OR" *> parseNotExpr)
            pure $ foldl ExprOr e1 es
        parseNotExpr = do
                (ExprNot <$> (symbolIs "NOT" *> parseExpr))
            <|> parseExpr
        parseExpr =
                parenthesized (ExprSelect <$> selectKW)
            <|> parenthesized parseAnd
            <|> (ExprExist <$> (symbolIs "exists" *> parseExpr))
            <|> (ExprBin <$> operand <*> operator <*> operand)
            <|> do c <- pMQColumnName
                   symbolIs "LIKE"
                   pat <- eatRet getString
                   return (ExprLike c pat)

        operand =
                (ExprCol <$> pMQColumnName)
            <|> (ExprValue <$> value)

        operator = (operatorIs "="  *> pure ExprEQ)
               <|> (operatorIs "<>" *> pure ExprNE)
               <|> (operatorIs ">=" *> pure ExprGE)
               <|> (operatorIs "<=" *> pure ExprLE)
               <|> (operatorIs ">"  *> pure ExprGT)
               <|> (operatorIs "<"  *> pure ExprLT)

    groupBy = do
        symbolIs "GROUP" *> symbolIs "BY"
        col <- pMQColumnName
        pure $ GroupBy col
    orderBy = do
        symbolIs "ORDER" *> symbolIs "BY"
        OrderBy <$> (colOrder `sepBy1` isComma)
    colOrder = do
        c <- pMQColumnName
        o <- optional (symbolIs "DESC" *> pure Descendent)
        return (c, o)

    functionCall = FunctionCall <$> functionName
                                <*> parenthesized (value `sepBy` isComma)

downSize :: Integer -> Int
downSize n
    | n > toInteger (maxBound :: Int) = error "cannot transform integer to int: out of bounds"
    | otherwise                       = fromIntegral n

value :: Stream Atom Value
value =
        eatRet simpleValue
    <|> (ValueVar <$> (eatRet getSymbolNotKW `sepBy` isDot))
  where
    simpleValue (AtomString s) = Just $ ValueString s
    simpleValue (AtomInt s)    = Just $ ValueInt (read s)
    simpleValue (AtomBytea s)  = Just $ ValueBytea s
    simpleValue _              = Nothing

eqCI :: String -> String -> Bool
eqCI a b = map toLower a == map toLower b

eatAtom :: (Show a, Eq a) => a -> Stream a ()
eatAtom a = eat ((==) a)

functionName :: Stream Atom FunctionName
functionName = FunctionName <$> eatRet getSymbolNotKW

tableName :: Stream Atom TableName
tableName = TableName <$> eatRet getSymbolNotKW

pMQColumnName :: Stream Atom MQColumnName
pMQColumnName =
        ((,) <$> tableName <*> (isDot *> columnName)) >>= \(tn, cn) -> pure (MQColumnName (Just tn) cn)
    <|> (MQColumnName Nothing <$> columnName)

qColumnName :: Stream Atom QualifiedColumnName
qColumnName = QualifiedColumnName <$> tableName <*> (isDot *> columnName)

columnName :: Stream Atom ColumnName
columnName = ColumnName <$> (eatRet getSymbolNotKW)

symbolIs :: String -> Stream Atom ()
symbolIs s = eat (maybe False (eqCI s) . getSymbol)

operatorIs :: String -> Stream Atom ()
operatorIs s = eat (maybe False (eqCI s) . getOperator)

getSymbolNotKW :: Atom -> Maybe String
getSymbolNotKW (AtomSymbol a)
    | isKW a    = Nothing
    | otherwise = Just a
getSymbolNotKW _ = Nothing

isKW :: String -> Bool
isKW (map toUpper -> l) =
    elem l ["WHERE", "SELECT", "INSERT", "CREATE", "UPDATE"]

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

isDot :: Stream Atom ()
isDot = eat (== AtomDot)

-- generic combinator stuff

between :: Applicative f => f op -> f cl -> f a -> f a
between popen pclose p = popen *> p <* pclose

sepBy1 :: Alternative f => f a -> f sep -> f [a]
sepBy1 p sep = scan where scan = liftA2 (:) p ((sep *> scan) <|> pure [])

sepBy :: Alternative f => f a -> f sep -> f [a]
sepBy p s = liftA2 (:) p ((s *> sepBy1 p s) <|> pure []) <|> pure []
