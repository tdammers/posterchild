{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Database.Posterchild
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString)

-- * Query AST

data SelectQuery =
  SelectQuery
    { selectFrom :: !SelectSource
    , selectFields :: !SelectFields
    }
  deriving (Show, Read, Eq)

newtype TableName =
  TableName { tableNameText :: Text }
  deriving newtype (Show, Read, Eq, Ord, IsString)

newtype ColumnName =
  ColumnName { columnNameText :: Text }
  deriving newtype (Show, Read, Eq, Ord, IsString)

newtype ParamName =
  ParamName { paramNameText :: Text }
  deriving newtype (Show, Read, Eq, Ord, IsString)

data SqlValue
  = SqlString Text
  | SqlInt Integer
  | SqlBytes ByteString
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
  = SelectColumn !TableName !ColumnName
  | SelectAlias !ColumnName
  | SelectValue !SqlValue
  | SelectParam !ParamName
  | SelectSubquery !SelectQuery
  deriving (Show, Read, Eq)

-- * Query Constraints

data SqlType
  = SqlIntT
  | SqlTextT
  | SqlBytesT
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data QueryConstraint
  = TableExists !TableName
  | ColumnExists !TableName !ColumnName
  | FieldTypesMatch !Selectable !Selectable
  deriving (Show, Read, Eq)

getQueryConstraints :: SelectQuery -> Vector QueryConstraint
getQueryConstraints q =
  mconcat
    [ Vector.map TableExists . Vector.fromList . Set.toList $ getRequiredTables q
    , Vector.map (uncurry ColumnExists) . Vector.fromList . Set.toList $ getRequiredColumns q
    ]

getRequiredTables :: SelectQuery -> Set TableName
getRequiredTables q =
  let requiredFrom = case selectFrom q of
        SelectFromDual -> []
        SelectFromTable n -> [n]
        SelectFromSubquery s -> getRequiredTables s
      requiredFields = getFieldsRequiredTables $ selectFields q
  in requiredFrom <> requiredFields

getFieldsRequiredTables :: SelectFields -> Set TableName
getFieldsRequiredTables (SelectFields fields) =
  Vector.foldl (<>) [] $ Vector.map getFieldRequiredTables fields
getFieldsRequiredTables SelectStar = []

getFieldRequiredTables :: SelectField -> Set TableName
getFieldRequiredTables field = case selectFieldSelectable field of
  SelectColumn tn _ -> [tn]
  SelectSubquery s -> getRequiredTables s
  _ -> []

getRequiredColumns :: SelectQuery -> Set (TableName, ColumnName)
getRequiredColumns q =
  let fromColumns = case selectFrom q of
        SelectFromSubquery s -> getRequiredColumns s
        _ -> []
      fromFields = case selectFields q of
        SelectFields fields -> Vector.foldl (<>) [] $ Vector.map getFieldRequiredColumns fields
        _ -> []
  in fromColumns <> fromFields

getFieldRequiredColumns :: SelectField -> Set (TableName, ColumnName)
getFieldRequiredColumns field = case selectFieldSelectable field of
  SelectColumn tn cn -> [(tn, cn)]
  SelectSubquery s -> getRequiredColumns s
  _ -> []
