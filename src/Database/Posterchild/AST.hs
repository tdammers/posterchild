{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Database.Posterchild.AST
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vector (Vector)
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
  = SqlText Text
  | SqlInt Integer
  | SqlBytes ByteString
  deriving (Show, Read, Eq)

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

data Comparison
  = Equals
  | NotEquals
  | Less
  | Greater
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

