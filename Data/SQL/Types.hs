module Data.SQL.Types
    ( ColumnName(..)
    , FunctionName(..)
    , TableName(..)
    , FunctionCall(..)
    , WhereExpr(..)
    , ExprBinOp(..)
    , Expr(..)
    , ExprOperand(..)
    , Value(..)
    , SelectorCol(..)
    , Selector(..)
    , ColumnDecl(..)
    , ColumnType(..)
    , ColumnNumericType(..)
    , ColumnStringType(..)
    , ColumnConstraint(..)
    , Query(..)
    ) where

newtype ColumnName = ColumnName String
    deriving (Show,Eq)

newtype FunctionName = FunctionName String
    deriving (Show,Eq)

newtype TableName = TableName String
    deriving (Show,Eq)

data FunctionCall = FunctionCall FunctionName [Value]
    deriving (Show,Eq)

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
    | ExprLike ColumnName String
    | ExprBin Value ExprBinOp Value
    deriving (Show,Eq)

data ExprOperand =
      ExprCol ColumnName
    | ExprValue Value
    deriving (Show,Eq)

data Selector =
      SelectorStar
    | SelectorCols [SelectorCol]
    deriving (Show,Eq)

type As = ColumnName

data SelectorCol =
      SelectorColName ColumnName (Maybe As)
    | SelectorColUdf FunctionCall
    deriving (Show,Eq)

newtype WhereExpr = WhereExpr Expr
    deriving (Show,Eq)

data Value =
      ValueBool Bool
    | ValueString String
    | ValueInt Integer
    | ValueVar String
    deriving (Show,Eq)

data ColumnDecl = ColumnDecl ColumnName ColumnType [ColumnConstraint]
    deriving (Show,Eq)

data ColumnType =
      ColumnNumeric ColumnNumericType
    | ColumnString ColumnStringType
    | ColumnByteArray
    | ColumnDate
    | ColumnTime
    | ColumnDateTime
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
    | Constraint_ForeignKey
    | Constraint_Default
    deriving (Show,Eq)

data Query =
      Select Selector TableName (Maybe WhereExpr)
    | Insert TableName (Maybe [ColumnName]) [Value]
    | Create TableName [ColumnDecl]
    deriving (Show,Eq)
