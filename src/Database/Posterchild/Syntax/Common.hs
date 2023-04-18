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
