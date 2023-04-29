{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Database.Posterchild.Syntax.Schema
where

import Data.Vector (Vector)
import Data.Map (Map)

import Database.Posterchild.Syntax.Common
import Database.Posterchild.Syntax.SqlTy

data Schema =
  Schema
    { schemaName :: SchemaName
    , schemaTables :: Map TableName Table
    }
    deriving (Show, Eq)

data Table =
  Table
    { tableColumns :: !(Vector Column)
    , tableConstraints :: !(Vector TableConstraint)
    }
    deriving (Show, Eq)

data Column =
  Column
    { columnName :: !ColumnName
    , columnType :: !SqlTy
    , columnNullable :: !Nullability
    }
    deriving (Show, Eq)

data TableConstraint
  = PrimaryKeyConstraint !(Vector ColumnName)
  | ForeignKeyConstraint !ForeignKey
  | UniqueConstraint !(Vector ColumnName)
  deriving (Show, Eq)

data ForeignKey =
  ForeignKey
    { fkForeignTable :: !TableName
    , fkColumnMapping :: !(Vector (ColumnName, ColumnName))
    , fkOnDelete :: ForeignKeyAction
    , fkOnUpdate :: ForeignKeyAction
    }
  deriving (Show, Eq)

data ForeignKeyAction
  = NoAction
  | Cascade
  | SetNull
  deriving (Show, Eq, Ord, Enum, Bounded)
