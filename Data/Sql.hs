module Data.Sql
    ( TableName(..)
    , FieldName(..)
    , CreateField(..)
    , FieldType
    , CreateConstraint
    , Query(..)
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
data FieldName = FieldName (Maybe String) String

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

data QueryVal =
      ValInt Integer
    | ValString String
    | ValNull

data Query =
      (:==:) FieldName QueryVal
    | (:/=:) FieldName QueryVal
    | (:~~:) FieldName String
    | (:>:) FieldName Integer
    | (:<:) FieldName Integer
    | (:>=:) FieldName Integer
    | (:<=:) FieldName Integer
    | (:||:) Query Query
    | (:&&:) Query Query

sqlCreate :: TableName -> [CreateField] -> String
sqlCreate (TableName t) l =
    "CREATE TABLE " ++ show t ++ " (" ++ bodyCreate ++ ")"
  where
    bodyCreate = intercalate ", " $ map toString l
    toString (Field name fty) =
        name ++ " " ++ fty
    toString (Constraint c) =
        c

sqlQuery :: Query -> String
sqlQuery = printQuery
  where
    printQuery (q1 :&&: q2)  = "((" ++ printQuery q1 ++ ") AND (" ++ printQuery q2 ++ "))"
    printQuery (q1 :||: q2)  = "((" ++ printQuery q1 ++ ") OR (" ++ printQuery q2 ++ "))"
    printQuery (f :==: v)    = show f ++ " = "  ++ sqlShowVal v
    printQuery (f :/=: v)    = show f ++ " != " ++ sqlShowVal v
    printQuery (f :<: v)     = show f ++ " < "  ++ showNum v
    printQuery (f :<=: v)    = show f ++ " <= " ++ showNum v
    printQuery (f :>: v)     = show f ++ " > "  ++ showNum v
    printQuery (f :>=: v)    = show f ++ " >= " ++ showNum v
    printQuery (f :~~: v)    = show f ++ " LIKE " ++ sqlShowString v

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