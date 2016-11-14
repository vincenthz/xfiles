module Data.SQL.Print
    ( pretty
    ) where

import Data.SQL.Types
import Data.Monoid
import Data.List

pretty :: Query -> String
pretty (Select s) = printSelect s
pretty (Insert t) = printInsert t
pretty (Create c) = printCreate c
pretty (Drop c) = printDrop c

printDrop :: Drop -> String
printDrop (DropTable ife tn) =
    "DROP TABLE " <> printIfe ife <> printTName tn

printSelect :: Select -> String
printSelect (SelectQuery sels sources mwhere mgroup morder) = unwords
    (["SELECT", (commaPrint $ map printSelector sels)
     ,"FROM", (commaPrint $ map printSources sources)
     ]
     ++ lMaybe mwhere (\(WhereExpr w) -> ["WHERE", printExpr w])
     ++ lMaybe mgroup (\(GroupBy cn) -> ["GROUP", "BY", printCName cn])
     ++ lMaybe morder (\(OrderBy ods) -> ["ORDER", "BY" ] ++ [commaPrint (map printOCol ods)])
     ++ []
    )
  where
    printOCol (cn, Nothing) = printCName cn
    printOCol (cn, (Just Ascendent)) = printCName cn
    printOCol (cn, (Just Descendent)) = printCName cn ++ " desc"

    printSelector (Selector scol mas) =
        printSelectorCol scol ++
        pMaybeStart " AS" printCName mas
    printSelectorCol SelectorColStar = "*"
    printSelectorCol (SelectorColName cname) = printCName cname
    printSelectorCol (SelectorColUdf (FunctionName fname) scols) =
        fname ++ parens (intercalate "," (map printSelectorCol scols))

    printSources (SourceTable tn mas) =
        printTName tn ++ maybe "" (\x -> " " ++ printTName x) mas

    printExpr (ExprAnd e1 e2) = printExpr e1 ++ " AND " ++ printExpr e2
    printExpr (ExprOr e1 e2) = printExpr e1 ++ " OR " ++ printExpr e2
    printExpr (ExprNot e1) = "NOT " ++ printExpr e1
    printExpr (ExprLike cname s) = printCName cname ++ " LIKE " ++ printString s
    printExpr (ExprBin v1 op v2) = printValue v1 ++ " " ++ printOp op ++ " " ++ printValue v2
    printExpr (ExprExist e) = "EXIST " ++ parens (printExpr e)
    printExpr (ExprSelect s) = printSelect s

    printOp ExprEQ = "="
    printOp ExprNE = "<>"
    printOp ExprGE = ">="
    printOp ExprLE = "<="
    printOp ExprGT = ">"
    printOp ExprLT = "<"

printInsert :: Insert -> String
printInsert (InsertQuery tn mcols mvals) = unwords
    (["INSERT","INTO"] ++ [printTName tn] ++
    maybe [] ((:[]) . parens . commaPrint . map printCName) mcols
    ++ ["VALUES"] ++ [parens (commaPrint $ map printValue mvals)]
    )

printCreate :: Create -> String
printCreate (CreateQuery ife (TableName tn) decls) =
    "CREATE TABLE " <> printIfe ife <> tn <> " (" <> cols <> ")"
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

    printConstraint Constraint_NotNull    = "NOT NULL"
    printConstraint Constraint_Unique     = "UNIQUE"
    printConstraint Constraint_PrimaryKey = "PRIMARY KEY"
    printConstraint Constraint_ForeignKey = "FOREIGN KEY"
    printConstraint Constraint_Default    = "DEFAULT"

printIfe :: Maybe IfNotExist -> String
printIfe Nothing = ""
printIfe (Just IfNotExist) = "IF NOT EXIST "

printCName :: ColumnName -> String
printCName (ColumnName [v]) = v
printCName (ColumnName l)   = intercalate "." l

printTName :: TableName -> String
printTName (TableName t)   = t

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

printString :: String -> String
printString s = "'" ++ s ++ "'"
