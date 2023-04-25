{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Database.Posterchild.Syntax.Abstract
where

import Data.Vector (Vector)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.String (IsString)


newtype TableName =
  TableName { tableNameText :: Text }
  deriving newtype (Show, Read, Eq, Ord, IsString)

newtype ColumnName =
  ColumnName { columnNameText :: Text }
  deriving newtype (Show, Read, Eq, Ord, IsString)

newtype ParamName =
  ParamName { paramNameText :: Text }
  deriving newtype (Show, Read, Eq, Ord, IsString)

data ColumnRef =
  ColumnRef !TableName !ColumnName
  deriving (Show, Read, Eq, Ord)

data SqlValue
  = SqlBool !Bool
  | SqlText !Text
  | SqlInt !Integer
  | SqlBytes !ByteString
  deriving (Show, Read, Eq)

data Binop
  = Equals
  | NotEquals
  | Less
  | Greater
  deriving (Show, Read, Eq)

data Unop
  = Not
  deriving (Show, Read, Eq)
data SelectQuery =
  SelectQuery
    { selectFrom :: !SelectFrom
    , selectFields :: !(Vector SelectField)
    , selectWhere :: !Expr
    }
  deriving (Show, Read, Eq)

data SelectField =
  SelectField
    { fieldExpr :: !Expr
    , fieldAlias :: !(Maybe ColumnName)
    }
  deriving (Show, Read, Eq)

data SelectFrom
  = SelectFromSingle !Tabloid !(Maybe TableName)
  | SelectJoin !JoinType !SelectFrom !SelectFrom !Expr
  deriving (Show, Read, Eq)

data Tabloid
  = DualTabloid
  | TableTabloid !TableName
  | SubqueryTabloid !SelectQuery
  deriving (Show, Read, Eq)

data JoinType
  = InnerJoin
  | LeftOuterJoin
  | RightOuterJoin
  | FullOuterJoin
  | CrossJoin
  deriving (Show, Read, Eq)

data Expr
  = NullE
  | BoolLitE !Bool
  | IntLitE !Integer
  | LitE !Text
  | RefE !(Maybe TableName) !ColumnName
  | ParamE !ParamName
  | UnopE !Unop Expr
  | BinopE !Binop !Expr !Expr
  | FoldE !BoolOp !(Vector Expr)
  | SubqueryE !SelectQuery
  deriving (Show, Read, Eq)

data BoolOp
  = Any
  | All
  deriving (Show, Read, Eq, Ord, Enum, Bounded)
