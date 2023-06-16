{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Database.Posterchild.Syntax.Select
where

import Data.Text (Text)
import Data.Vector (Vector)

import Database.Posterchild.Syntax.Common

-- TODO: figure out how to represent UNION / INTERSECT queries.

data SelectQuery =
  SelectQuery
    { selectFrom :: !SelectFrom
    , selectFields :: !(Vector SelectField)
    , selectWhere :: !Expr
    , selectGroupBy :: !(Vector SelectGroupItem)
    , selectHaving :: !Expr
    , selectSort :: !(Vector SelectSort)
    }
  deriving (Show, Read, Eq)

data SelectField =
  SelectField
    { fieldExpr :: !Expr
    , fieldAlias :: !(Maybe ColumnName)
    }
  deriving (Show, Read, Eq)

data SelectGroupItem
  = GroupByExpr !Expr
  deriving (Show, Read, Eq)

data AscDesc
  = SortAscending
  | SortDescending
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data SelectSort =
  SelectSort
    { sortBy :: !Expr
    , sortAscDesc :: !AscDesc
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

data Binop
  = Equals
  | NotEquals
  | Less
  | Greater
  | Plus
  | Minus
  | Mult
  | Div
  deriving (Show, Read, Eq)

data Unop
  = Not
  | UnaryPlus
  | UnaryMinus
  deriving (Show, Read, Eq)

data BoolOp
  = Any
  | All
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

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

