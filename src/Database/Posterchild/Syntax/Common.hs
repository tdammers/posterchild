{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Database.Posterchild.Syntax.Common
where

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text

newtype SchemaName =
  SchemaName { schemaNameText :: Text }
  deriving newtype (Show, Read, Eq, Ord, IsString)

newtype TableName =
  TableName { tableNameText :: Text }
  deriving newtype (Show, Read, Eq, Ord, IsString)

newtype ColumnName =
  ColumnName { columnNameText :: Text }
  deriving newtype (Show, Read, Eq, Ord, IsString)

newtype ParamName =
  ParamName { paramNameIndex :: Integer }
  deriving newtype (Show, Read, Eq, Ord)

paramNameText :: ParamName -> Text
paramNameText (ParamName i) = Text.pack . show $ i

data ColumnRef =
  ColumnRef !TableName !ColumnName
  deriving (Show, Read, Eq, Ord)

data Nullability
  = Null
  | NotNull
  deriving (Show, Read, Eq, Ord, Enum, Bounded)
