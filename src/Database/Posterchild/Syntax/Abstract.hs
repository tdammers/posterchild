{-# LANGUAGE DerivingStrategies #-}

module Database.Posterchild.Syntax.Abstract
where

import Data.Vector (Vector)

import Database.Posterchild.Syntax.Common

data SelectQuery =
  SelectQuery
    { selectFrom :: !SelectSource
    , selectFields :: !SelectFields
    , selectWhere :: !Where
    }
  deriving (Show, Read, Eq)

data Where
  = WhereTrue
  | WhereFalse
  | WhereNull !Selectable
  | WhereCompare !Comparison !Selectable !Selectable
  | WhereNot !Where
  | WhereAll !(Vector Where)
  | WhereAny !(Vector Where)
  deriving (Show, Read, Eq)

data SelectSource
  = SelectFromDual
  | SelectFromTable !TableName
  | SelectFromSubquery !SelectQuery
  deriving (Show, Read, Eq)

data SelectFields
  = SelectStar
  | SelectFields !(Vector SelectField)
  deriving (Show, Read, Eq)

data SelectField =
  SelectField
    { selectFieldSelectable :: !Selectable
    , selectFieldAlias :: !(Maybe ColumnName)
    }
  deriving (Show, Read, Eq)

data Selectable
  = SelectColumn !ColumnRef
  | SelectAlias !ColumnName
  | SelectValue !SqlValue
  | SelectParam !ParamName
  | SelectSubquery !SelectQuery
  deriving (Show, Read, Eq)

