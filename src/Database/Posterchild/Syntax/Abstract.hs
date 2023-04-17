{-# LANGUAGE DerivingStrategies #-}

module Database.Posterchild.Syntax.Abstract
where

import Data.Vector (Vector)
import Data.Text (Text)

import Database.Posterchild.Syntax.Common

data SelectQuery =
  SelectQuery
    { selectFrom :: !SelectFrom
    , selectFields :: !SelectFields
    , selectWhere :: !Expr
    }
  deriving (Show, Read, Eq)

data Expr
  = TrueE
  | FalseE
  | NullE
  | LitE !Text
  | ColumnE !ColumnRef
  | AliasE !ColumnName
  | ParamE !ParamName
  | SubqueryE !SelectQuery
  | IsNullE !Expr
  | BinopE !Binop !Expr !Expr
  | NotE !Expr
  | AllE !(Vector Expr)
  | AnyE !(Vector Expr)
  deriving (Show, Read, Eq)

data SelectFrom = 
  SelectFrom
    { selectFromSource :: !SelectSource
    , selectFromAs :: !TableName
    }
  deriving (Show, Read, Eq)

data SelectSource
  = SelectFromDual
  | SelectFromTable !TableName
  | SelectFromSubquery !SelectQuery
  deriving (Show, Read, Eq)

newtype SelectFields
  = SelectFields (Vector SelectField)
  deriving (Show, Read, Eq)

data SelectField =
  SelectField
    { selectFieldSelectable :: !Expr
    , selectFieldAlias :: !(Maybe ColumnName)
    }
  deriving (Show, Read, Eq)
