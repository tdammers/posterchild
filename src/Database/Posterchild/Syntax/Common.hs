{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Database.Posterchild.Syntax.Common
where

import Data.ByteString (ByteString)
import Data.Text (Text)
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

data Comparison
  = Equals
  | NotEquals
  | Less
  | Greater
  deriving (Show, Read, Eq)

