{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}

module Database.Posterchild.SchemaConstraints
where

import Data.Kind
import GHC.TypeLits

class SchemaHasTable (schema :: Type) (tableName :: Symbol) where
  type (SchemaTableTy schema tableName) :: Type

class TableHasColumn (table :: Type) (columnName :: Symbol) where
  type (TableColumnTy table columnName) :: Type
