{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}

module Database.Posterchild
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (IsString)

-- * Query AST

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

-- * Query Constraints

data SqlTy
  = SqlIntT
  | SqlTextT
  | SqlBytesT
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data ColumnRef =
  ColumnRef !TableName !ColumnName
  deriving (Show, Read, Eq, Ord)

data QueryConstraint
  = TableExists !TableName
  | ColumnExists !ColumnRef
  | CompatibleTypes !Ty !Ty
  deriving (Show, Read, Eq)

data Ty
  = MonoTy SqlTy
  | ColumnTyRef !ColumnRef
  | ParamTyRef !ParamName
  deriving (Show, Read, Eq)

type AliasTable =
  Map ColumnName Selectable

resolveAlias :: AliasTable -> ColumnName -> Maybe Selectable
resolveAlias aliases alias =
  case Map.lookup alias aliases of
    Nothing ->
      Nothing
    Just (SelectAlias alias') ->
      resolveAlias aliases alias'
    Just x ->
      Just x

getQueryAliases :: SelectQuery -> AliasTable
getQueryAliases q =
  let fieldAliases = case selectFields q of
        SelectStar -> error "This doesn't work yet"
        SelectFields fields -> do
          Vector.foldl (<>) [] (Vector.map getSelectFieldAliases fields)
      whereAliases = getWhereAliases $ selectWhere q
  in fieldAliases <> whereAliases

getWhereAliases :: Where -> AliasTable
getWhereAliases (WhereCompare _ a b) =
  getSelectableAliases a <> getSelectableAliases b
getWhereAliases (WhereNot w) =
  getWhereAliases w
getWhereAliases (WhereAll ws) =
  Vector.foldl (<>) [] $ Vector.map getWhereAliases ws
getWhereAliases (WhereAny ws) =
  Vector.foldl (<>) [] $ Vector.map getWhereAliases ws
getWhereAliases (WhereNull selectable) =
  getSelectableAliases selectable
getWhereAliases _ = []

getSelectableAliases :: Selectable -> AliasTable
getSelectableAliases (SelectSubquery s) = getQueryAliases s
getSelectableAliases _ = []

getSelectFieldAliases :: SelectField -> AliasTable
getSelectFieldAliases (SelectField _ Nothing) = []
getSelectFieldAliases (SelectField selectable (Just alias)) =
  [(alias, selectable)] <> getSelectableAliases selectable

getSelectableType :: AliasTable -> Selectable -> Maybe Ty
getSelectableType _ (SelectColumn colref) =
  return $ ColumnTyRef colref
getSelectableType aliases (SelectAlias a) = do
  selectable <- Map.lookup a aliases
  getSelectableType aliases selectable
getSelectableType _ (SelectValue v) =
  Just $ getValueType v
getSelectableType _ (SelectParam p) =
  Just $ ParamTyRef p
getSelectableType aliases (SelectSubquery s) = do
  subResults <- getQueryResultType aliases s
  case subResults of
    [ty] -> Just ty
    _ -> Nothing

getQueryResultType :: AliasTable -> SelectQuery -> Maybe (Vector Ty)
getQueryResultType aliases q =
  case selectFields q of
    SelectStar ->
      error "This doesn't work yet"
    SelectFields fields -> do
      Vector.forM fields $ getSelectableType aliases . selectFieldSelectable

getValueType :: SqlValue -> Ty
getValueType (SqlInt _) = MonoTy SqlIntT
getValueType (SqlText _) = MonoTy SqlTextT
getValueType (SqlBytes _) = MonoTy SqlBytesT

getQueryConstraints :: AliasTable -> SelectQuery -> Vector QueryConstraint
getQueryConstraints aliases q =
  mconcat
    [ Vector.map TableExists . Vector.fromList . Set.toList $ getRequiredTables q
    , Vector.map ColumnExists . Vector.fromList . Set.toList $ getRequiredColumns q
    , getTypeConstraints aliases q
    ]

getTypeConstraints :: AliasTable -> SelectQuery -> Vector QueryConstraint
getTypeConstraints aliases q =
  let fromConstrs = case selectFrom q of
                      SelectFromSubquery s ->
                        getTypeConstraints aliases s
                      _ ->
                        []
      fieldConstrs = case selectFields q of
        SelectStar ->
          error "This doesn't work yet"
        SelectFields fields ->
          Vector.foldl (<>) [] $ Vector.map (getSelectableTypeConstraints aliases . selectFieldSelectable) fields
      whereConstrs = getWhereTypeConstraints aliases $ selectWhere q
  in fromConstrs <> fieldConstrs <> whereConstrs

getWhereTypeConstraints :: AliasTable -> Where -> Vector QueryConstraint
getWhereTypeConstraints aliases (WhereCompare _ a b) =
  let result = (,) <$> getSelectableType aliases a <*> getSelectableType aliases b
  in case result of
    Nothing ->
      []
    Just (aTy, bTy) ->
      [CompatibleTypes aTy bTy]
getWhereTypeConstraints aliases (WhereNot w) =
  getWhereTypeConstraints aliases w
getWhereTypeConstraints aliases (WhereAll ws) =
  Vector.concatMap (getWhereTypeConstraints aliases) ws
getWhereTypeConstraints aliases (WhereAny ws) =
  Vector.concatMap (getWhereTypeConstraints aliases) ws
getWhereTypeConstraints _ _ = []

getSelectableTypeConstraints :: AliasTable -> Selectable -> Vector QueryConstraint
getSelectableTypeConstraints aliases (SelectSubquery s) =
  getQueryConstraints aliases s
getSelectableTypeConstraints _ _ = []

getRequiredTables :: SelectQuery -> Set TableName
getRequiredTables q =
  let requiredFrom = case selectFrom q of
        SelectFromDual -> []
        SelectFromTable n -> [n]
        SelectFromSubquery s -> getRequiredTables s
      requiredFields = getFieldsRequiredTables $ selectFields q
      requiredWhere = getWhereRequiredTables $ selectWhere q
  in requiredFrom <> requiredFields <> requiredWhere

getWhereRequiredTables :: Where -> Set TableName
getWhereRequiredTables (WhereCompare _ a b) =
  getSelectableRequiredTables a <> getSelectableRequiredTables b
getWhereRequiredTables (WhereAll ws) =
  Vector.foldl (<>) [] $ Vector.map getWhereRequiredTables ws
getWhereRequiredTables (WhereAny ws) =
  Vector.foldl (<>) [] $ Vector.map getWhereRequiredTables ws
getWhereRequiredTables (WhereNot w) =
  getWhereRequiredTables w
getWhereRequiredTables _ =
  []

getFieldsRequiredTables :: SelectFields -> Set TableName
getFieldsRequiredTables (SelectFields fields) =
  Vector.foldl (<>) [] $ Vector.map getFieldRequiredTables fields
getFieldsRequiredTables SelectStar = []

getSelectableRequiredTables :: Selectable -> Set TableName
getSelectableRequiredTables = \case
  SelectColumn (ColumnRef tn _) -> [tn]
  SelectSubquery s -> getRequiredTables s
  _ -> []

getFieldRequiredTables :: SelectField -> Set TableName
getFieldRequiredTables field =
  getSelectableRequiredTables (selectFieldSelectable field)

getRequiredColumns :: SelectQuery -> Set ColumnRef
getRequiredColumns q =
  let fromColumns = case selectFrom q of
        SelectFromSubquery s -> getRequiredColumns s
        _ -> []
      fromFields = case selectFields q of
        SelectFields fields -> Vector.foldl (<>) [] $ Vector.map getFieldRequiredColumns fields
        _ -> []
      fromWhere = getWhereRequiredColumns $ selectWhere q
  in fromColumns <> fromFields <> fromWhere

getWhereRequiredColumns :: Where -> Set ColumnRef
getWhereRequiredColumns (WhereCompare _ a b) =
  getSelectableRequiredColumns a <> getSelectableRequiredColumns b
getWhereRequiredColumns (WhereAll ws) =
  Vector.foldl (<>) [] $ Vector.map getWhereRequiredColumns ws
getWhereRequiredColumns (WhereAny ws) =
  Vector.foldl (<>) [] $ Vector.map getWhereRequiredColumns ws
getWhereRequiredColumns (WhereNot w) =
  getWhereRequiredColumns w
getWhereRequiredColumns _ =
  []

getFieldRequiredColumns :: SelectField -> Set ColumnRef
getFieldRequiredColumns field =
  getSelectableRequiredColumns $ selectFieldSelectable field

getSelectableRequiredColumns :: Selectable -> Set ColumnRef
getSelectableRequiredColumns = \case
  SelectColumn colref -> [colref]
  SelectSubquery s -> getRequiredColumns s
  _ -> []
