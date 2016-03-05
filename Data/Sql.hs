module Data.Sql
    ( TableName(..)
    , FieldName(..)
    , CreateField(..)
    , FieldType
    , CreateConstraint
    , Query(..)
    , WhereQuery(..)
    , QueryVal(..)
    -- methods
    , sqlFN
    , sqlFQFN
    , sqlCreate
    , sqlQuery
    , sqlShowString
    )
    where

import Data.List

newtype TableName = TableName String
    deriving (Eq)
data FieldName = FieldName (Maybe String) String
    deriving (Eq)

sqlFN   :: String -> FieldName
sqlFN = FieldName Nothing

sqlFQFN :: TableName -> String -> FieldName
sqlFQFN (TableName tn) s = FieldName (Just tn) s

instance Show FieldName where
    show (FieldName Nothing f) = f -- SQL escape
    show (FieldName (Just tn) f) = tn ++ "." ++ f -- SQL escape
instance Show TableName where
    show (TableName f) = f -- SQL escape

type FieldType = String
type CreateConstraint = String

data CreateField = Field String FieldType
                 | Constraint CreateConstraint
                 deriving (Show,Eq)

data QueryVal =
      ValInt Integer
    | ValString String
    | ValNull
    deriving (Show,Eq)

data Query =
      Select [FieldName] TableName WhereQuery
    | Union Query Query
    | Intersection Query Query
    deriving (Show,Eq)

data WhereQuery =
      (:==:) FieldName QueryVal
    | (:/=:) FieldName QueryVal
    | (:~~:) FieldName String
    | (:>:) FieldName Integer
    | (:<:) FieldName Integer
    | (:>=:) FieldName Integer
    | (:<=:) FieldName Integer
    | (:||:) WhereQuery WhereQuery
    | (:&&:) WhereQuery WhereQuery
    | InIntegers FieldName [Integer]
    | InStrings FieldName [String]
    | InQuery FieldName Query
    | EqQuery FieldName Query
    deriving (Show,Eq)

sqlCreate :: TableName -> [CreateField] -> String
sqlCreate (TableName t) l =
    "CREATE TABLE " ++ show t ++ " (" ++ bodyCreate ++ ")"
  where
    bodyCreate = intercalate ", " $ map toString l
    toString (Field name fty) =
        name ++ " " ++ fty
    toString (Constraint c) =
        c

sqlQuery :: WhereQuery -> String
sqlQuery = printWQuery
  where
    printWQuery (q1 :&&: q2)  = "((" ++ printWQuery q1 ++ ") AND (" ++ printWQuery q2 ++ "))"
    printWQuery (q1 :||: q2)  = "((" ++ printWQuery q1 ++ ") OR (" ++ printWQuery q2 ++ "))"
    printWQuery (f :==: v)    = show f ++ " = "  ++ sqlShowVal v
    printWQuery (f :/=: v)    = show f ++ " != " ++ sqlShowVal v
    printWQuery (f :<: v)     = show f ++ " < "  ++ showNum v
    printWQuery (f :<=: v)    = show f ++ " <= " ++ showNum v
    printWQuery (f :>: v)     = show f ++ " > "  ++ showNum v
    printWQuery (f :>=: v)    = show f ++ " >= " ++ showNum v
    printWQuery (f :~~: v)    = show f ++ " LIKE " ++ sqlShowString v
    printWQuery (f `InIntegers` []) = error ("empty set of integer for field : " ++ show f)
    printWQuery (f `InIntegers` v)  = show f ++ " IN (" ++ intercalate ", " (map showNum v) ++ ")"
    printWQuery (f `InStrings` [])  = error ("empty set of integer for field : " ++ show f)
    printWQuery (f `InStrings` v)   = show f ++ " IN (" ++ intercalate ", " (map sqlShowString v) ++ ")"
    printWQuery (f `InQuery` v)     = show f ++ " IN (" ++ printQuery v ++ ")"
    printWQuery (f `EqQuery` v)     = show f ++ " = (" ++ printQuery v ++ ")"

    printQuery (Select fns tb whereq) =
        "SELECT " ++ intercalate "," (map show fns) ++ " FROM " ++ show tb ++ " WHERE " ++ printWQuery whereq
    printQuery (Union q1 q2) =
        printQuery q1 ++ " UNION " ++ printQuery q2
    printQuery (Intersection q1 q2) =
        printQuery q1 ++ " INTERSECT " ++ printQuery q2

    showNum :: Integer -> String
    showNum = show

    sqlShowVal (ValInt i)    = showNum i
    sqlShowVal (ValString s) = sqlShowString s
    sqlShowVal ValNull       =  "NULL"

sqlShowString :: String  -> String
sqlShowString s = '\'' : loop s
  where
    -- end with a quote for "efficiency" of not re-doing the string
    loop []        = ['\'']
    loop ('\'':xs) = '\'' : '\'' : loop xs
    loop (x:xs)    = x:loop xs
