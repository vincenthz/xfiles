{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.SQL.Types
    ( ColumnName(..)
    , QualifiedColumnName(..)
    , MQColumnName(..)
    , FunctionName(..)
    , TableName(..)
    , AliasName
    , FunctionCall(..)
    , WhereExpr(..)
    , ExprBinOp(..)
    , Expr(..)
    , ExprOperand(..)
    , Value(..)
    , SelectorCol(..)
    , Selector(..)
    , Select(..)
    , SelectSource(..)
    , OrderBy(..)
    , Order(..)
    , GroupBy(..)
    , ColumnDecl(..)
    , ColumnType(..)
    , ColumnNumericType(..)
    , ColumnStringType(..)
    , ColumnConstraint(..)
    , Insert(..)
    , Create(..)
    , Query(..)
    , Update(..)
    , Drop(..)
    , IfNotExist(..)
    ) where

import Data.ByteString (ByteString)
import Data.String

newtype ColumnName = ColumnName String
    deriving (Show,Eq,IsString)

data QualifiedColumnName = QualifiedColumnName TableName ColumnName
    deriving (Show,Eq)

data MQColumnName = MQColumnName (Maybe TableName) ColumnName
    deriving (Show,Eq)

instance IsString MQColumnName where
    fromString s = case wordsBy (== '.') s of
        []    -> error "invalid mq column name: empty"
        [x]   -> MQColumnName Nothing (fromString x)
        [x,y] -> MQColumnName (Just $ fromString x) (fromString y)
        _     -> error ("invalid mq column name: " ++ show s)
      where
        wordsBy :: (Char -> Bool) -> String -> [String]
        wordsBy isDelim xs =
          case dropWhile isDelim xs of
            ""  -> []
            xs' -> w : wordsBy isDelim rest
                  where (w, rest) = break isDelim xs'

instance IsString QualifiedColumnName where
    fromString s = case wordsBy (== '.') s of
        []    -> error "invalid qualified column name: empty"
        [x,y] -> QualifiedColumnName (fromString x) (fromString y)
        _     -> error ("invalid qualified column name: " ++ show s)
      where
        wordsBy :: (Char -> Bool) -> String -> [String]
        wordsBy isDelim xs =
          case dropWhile isDelim xs of
            ""  -> []
            xs' -> w : wordsBy isDelim rest
                  where (w, rest) = break isDelim xs'

newtype FunctionName = FunctionName String
    deriving (Show,Eq,IsString)

newtype TableName = TableName String
    deriving (Show,Eq,IsString)

type AliasName = TableName

data FunctionCall = FunctionCall FunctionName [Value]
    deriving (Show,Eq)

data Value =
      ValueBool Bool
    | ValueString String
    | ValueInt Integer
    | ValueBytea ByteString
    | ValueVar [String]
    | ValueStar
    deriving (Show,Eq)

type As = ColumnName

data SelectSource =
      SourceTable TableName (Maybe AliasName)
    deriving (Show,Eq)

-- | IF NOT EXIST qualifier for DROP and CREATE
data IfNotExist = IfNotExist
    deriving (Show,Eq)

data Order = Ascendent | Descendent
    deriving (Show,Eq)

------------------------------------------------------------------------

data ExprBinOp =
      ExprEQ
    | ExprNE
    | ExprGE
    | ExprLE
    | ExprGT
    | ExprLT
    deriving (Show,Eq)

data Expr =
      ExprAnd Expr Expr
    | ExprOr Expr Expr
    | ExprNot Expr
    | ExprLike MQColumnName String
    | ExprBin ExprOperand ExprBinOp ExprOperand
    | ExprExist Expr
    | ExprSelect Select
    deriving (Show,Eq)

data ExprOperand =
      ExprCol MQColumnName
    | ExprValue Value
    deriving (Show,Eq)

------------------------------------------------------------------------

data Selector = Selector SelectorCol (Maybe As)
    deriving (Show,Eq)

data SelectorCol =
      SelectorColName MQColumnName
    | SelectorColUdf FunctionName [SelectorCol]
    | SelectorColStar
    deriving (Show,Eq)

newtype WhereExpr = WhereExpr Expr
    deriving (Show,Eq)

data GroupBy = GroupBy MQColumnName
    deriving (Show,Eq)

data OrderBy = OrderBy [(MQColumnName,Maybe Order)]
    deriving (Show,Eq)

------------------------------------------------------------------------

-- | Column declaration in CREATE
data ColumnDecl = ColumnDecl ColumnName ColumnType [ColumnConstraint]
    deriving (Show,Eq)

data ColumnType =
      ColumnNumeric ColumnNumericType
    | ColumnString ColumnStringType
    | ColumnByteArray
    | ColumnDate
    | ColumnTime
    | ColumnDateTime
    | ColumnFunctionUnknown String [Value]
    | ColumnUnknown String
    deriving (Show,Eq)

data ColumnNumericType =
      CNum_Int16
    | CNum_Int32
    | CNum_Int64
    | CNum_Integer Int -- number of digits
    | CNum_Decimal Int Int -- number of digits before and after decimal
    | CNum_FP32
    | CNum_FP64
    deriving (Show,Eq)

data ColumnStringType =
      CStr_Char Int
    | CStr_VarChar Int
    deriving (Show,Eq)

data ColumnConstraint =
      Constraint_NotNull
    | Constraint_Unique
    | Constraint_PrimaryKey
    | Constraint_ForeignKey QualifiedColumnName
    | Constraint_Default
    | Constraint_UnknownFunction String [String]
    | Constraint_Unknown String
    deriving (Show,Eq)

------------------------------------------------------------------------

data Create = CreateQuery (Maybe IfNotExist) TableName [ColumnDecl]
    deriving (Show,Eq)

data Drop = DropTable (Maybe IfNotExist) TableName
    deriving (Show,Eq)

data Update = UpdateQuery TableName [(ColumnName, Value)] (Maybe WhereExpr)
    deriving (Show,Eq)

data Insert = InsertQuery TableName
                          (Maybe [ColumnName])
                          [Value]
    deriving (Show,Eq)

data Select = SelectQuery [Selector]
                          [SelectSource]
                          (Maybe WhereExpr)
                          (Maybe GroupBy)
                          (Maybe OrderBy)
    deriving (Show,Eq)

------------------------------------------------------------------------

data Query =
      Select Select
    | Insert Insert
    | Create Create
    | Drop Drop
    | Update Update
    deriving (Show,Eq)
