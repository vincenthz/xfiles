module Data.SQL.Print
    ( pretty
    ) where

import Data.SQL.Types
import Data.Monoid
import Data.List
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteArray.Encoding as B

pretty :: Query -> String
pretty (Select s) = printSelect s
pretty (Insert t) = printInsert t
pretty (Create c) = printCreate c
pretty (Drop c) = printDrop c
pretty (Update c) = printUpdate c

printDrop :: Drop -> String
printDrop (DropTable ife tn) =
    "DROP TABLE " <> printIfe ife <> printTName tn

printUpdate :: Update -> String
printUpdate (UpdateQuery tn cvals mwhere) = unwords
    (["UPDATE", printTName tn, "SET"]
     ++ [commaPrint (map printCVal cvals)]
     ++ lMaybe mwhere (\(WhereExpr w) -> ["WHERE", printExpr w])
    )
  where
    printCVal (cn, val) = printCName cn ++ "=" ++ printValue val

printSelect :: Select -> String
printSelect (SelectQuery sels sources mwhere mgroup morder) = unwords
    (["SELECT", (commaPrint $ map printSelector sels)
     ,"FROM", (commaPrint $ map printSources sources)
     ]
     ++ lMaybe mwhere (\(WhereExpr w) -> ["WHERE", printExpr w])
     ++ lMaybe mgroup (\(GroupBy cn) -> ["GROUP", "BY", printMQCName cn])
     ++ lMaybe morder (\(OrderBy ods) -> ["ORDER", "BY" ] ++ [commaPrint (map printOCol ods)])
     ++ []
    )
  where
    printOCol (cn, Nothing) = printMQCName cn
    printOCol (cn, (Just Ascendent)) = printMQCName cn
    printOCol (cn, (Just Descendent)) = printMQCName cn ++ " desc"

    printSelector (Selector scol mas) =
        printSelectorCol scol ++
        pMaybeStart " AS" printCName mas
    printSelectorCol (SelectorColStar Nothing) = "*"
    printSelectorCol (SelectorColStar (Just tn)) = printTName tn ++ "." ++ "*"
    printSelectorCol (SelectorColName cname) = printMQCName cname
    printSelectorCol (SelectorColUdf (FunctionName fname) scols) =
        fname ++ parens (intercalate "," (map printSelectorCol scols))

    printSources (SourceTable tn mas) =
        printTName tn ++ maybe "" (\x -> " " ++ printTName x) mas

printInsert :: Insert -> String
printInsert (InsertQuery tn mcols mvals) = unwords
    (["INSERT","INTO"] ++ [printTName tn] ++
    maybe [] ((:[]) . parens . commaPrint . map printCName) mcols
    ++ ["VALUES"] ++ [parens (commaPrint $ map printValue mvals)]
    )

printCreate :: Create -> String
printCreate (CreateQuery ife (TableName tn) decls) =
    "CREATE TABLE " <> printIfne ife <> tn <> " (" <> cols <> ")"
  where
    cols = commaPrint  $ map printDecl decls

    printDecl (ColumnDecl cname ty constraints) =
        printCName cname ++ " " ++
        printType ty ++ " " ++
        intercalate " " (map printConstraint constraints)

    printType ColumnByteArray = "BYTEA"
    printType (ColumnString (CStr_Char n)) = "CHAR(" <> show n <> ")"
    printType (ColumnString (CStr_VarChar n)) = "VARCHAR(" <> show n <> ")"
    printType (ColumnNumeric CNum_Int16) = "SHORTINT"
    printType (ColumnNumeric CNum_Int32) = "INT"
    printType (ColumnNumeric CNum_Int64) = "BIGINT"
    printType (ColumnNumeric (CNum_Integer n)) = "NUMERIC(" <> show n <> ")"
    printType (ColumnNumeric (CNum_Decimal n1 0)) = "NUMERIC(" <> show n1 <> ")"
    printType (ColumnNumeric (CNum_Decimal n1 n2)) = "NUMERIC(" <> show n1 <> "," <> show n2 <> ")"
    printType (ColumnNumeric CNum_FP32) = "FLOAT"
    printType (ColumnNumeric CNum_FP64) = "DOUBLE"
    printType (ColumnDate) = "DATE"
    printType (ColumnDateTime) = "DATETIME"
    printType (ColumnTime) = "TIME"
    printType (ColumnFunctionUnknown f params) = f <> "(" <> commaPrint (map printValue params) <> ")"
    printType (ColumnUnknown s) = s

    printConstraint Constraint_NotNull    = "NOT NULL"
    printConstraint Constraint_Unique     = "UNIQUE"
    printConstraint Constraint_PrimaryKey = "PRIMARY KEY"
    printConstraint (Constraint_References (QualifiedColumnName ftn fcn)) =
        "REFERENCES " <> printTName ftn <> "(" <> printCName fcn <> ")"
    printConstraint Constraint_Default    = "DEFAULT"
    printConstraint (Constraint_UnknownFunction f p) = f <> "(" <> commaPrint p <>  ")"
    printConstraint (Constraint_Unknown s) = s

printExpr :: Expr -> String
printExpr (ExprAnd e1 e2) = printExpr e1 ++ " AND " ++ printExpr e2
printExpr (ExprOr e1 e2) = printExpr e1 ++ " OR " ++ printExpr e2
printExpr (ExprNot e1) = "NOT " ++ printExpr e1
printExpr (ExprLike cname s) = printMQCName cname ++ " LIKE " ++ printString s
printExpr (ExprBin v1 op v2) = printOperand v1 ++ " " ++ printOp op ++ " " ++ printOperand v2
printExpr (ExprExist e) = "EXIST " ++ parens (printExpr e)
printExpr (ExprSelect s) = printSelect s

printOperand :: ExprOperand -> String
printOperand (ExprCol c) = printMQCName c
printOperand (ExprValue v) = printValue v

printOp :: ExprBinOp -> String
printOp ExprEQ = "="
printOp ExprNE = "<>"
printOp ExprGE = ">="
printOp ExprLE = "<="
printOp ExprGT = ">"
printOp ExprLT = "<"

printIfne :: Maybe IfNotExist -> String
printIfne Nothing = ""
printIfne (Just IfNotExist) = "IF NOT EXISTS "

printIfe :: Maybe IfExist -> String
printIfe Nothing = ""
printIfe (Just IfExist) = "IF EXISTS "

printMQCName :: MQColumnName -> String
printMQCName (MQColumnName (Just t) v) = printTName t ++ "." ++ printCName v
printMQCName (MQColumnName Nothing v) = printCName v

printQualifiedCName :: QualifiedColumnName -> String
printQualifiedCName (QualifiedColumnName tn cn) = printTName tn ++ "." ++ printCName cn

printCName :: ColumnName -> String
printCName (ColumnName c) = c

printTName :: TableName -> String
printTName (TableName t) = t

commaPrint :: [String] -> String
commaPrint = intercalate ", "

pMaybeStart :: String -> (a -> String) -> Maybe a -> String
pMaybeStart _     _ Nothing  = ""
pMaybeStart start f (Just j) = start ++ " " ++ f j

parens :: String -> String
parens s = "(" ++ s ++ ")"

lMaybe :: Maybe a -> (a -> [b]) -> [b]
lMaybe Nothing  _ = []
lMaybe (Just l) f = f l

printValue :: Value -> String
printValue (ValueBool True) = "TRUE"
printValue (ValueBool False) = "FALSE"
printValue (ValueString s) = printString s
printValue (ValueInt i) = show i
printValue (ValueVar v) = intercalate "." v
printValue (ValueStar) = "*"
printValue (ValueBytea bs) = "E\'\\x" ++ BC.unpack (B.convertToBase B.Base16 bs) ++ "\'"

printString :: String -> String
printString s = "'" ++ s ++ "'"
