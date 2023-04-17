{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Database.Posterchild.TyCheck
where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isDigit)

import Database.Posterchild.Syntax.Common
import Database.Posterchild.Syntax.Abstract

data SqlTy
  = SqlBoolT
  | SqlIntT
  | SqlTextT
  | SqlBytesT
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data QueryConstraint
  = TableExists !TableName
  | ColumnExists !ColumnRef
  | CompatibleTypes !Ty !Ty
  deriving (Show, Read, Eq)

data Ty
  = NullTy
  | MonoTy SqlTy
  | ColumnTyRef !ColumnRef
  | ParamTyRef !ParamName
  deriving (Show, Read, Eq, Ord)

type AliasTable =
  Map ColumnName Expr

resolveAlias :: AliasTable -> ColumnName -> Either String Expr
resolveAlias aliases alias =
  case Map.lookup alias aliases of
    Nothing ->
      return $ AliasE alias
    Just (AliasE alias') ->
      if alias' == alias then
        return $ AliasE alias
      else
        resolveAlias aliases alias'
    Just x ->
      return x

getQueryAliases :: SelectQuery -> AliasTable
getQueryAliases q =
  let fieldAliases = case selectFields q of
        SelectFields fields -> do
          Vector.foldl (<>) [] (Vector.map getSelectFieldAliases fields)
      whereAliases = getExprAliases $ selectWhere q
  in fieldAliases <> whereAliases

getExprAliases :: Expr -> AliasTable
getExprAliases (BinopE _ a b) =
  getExprAliases a <> getExprAliases b
getExprAliases (NotE w) =
  getExprAliases w
getExprAliases (AllE ws) =
  Vector.foldl (<>) [] $ Vector.map getExprAliases ws
getExprAliases (AnyE ws) =
  Vector.foldl (<>) [] $ Vector.map getExprAliases ws
getExprAliases (IsNullE selectable) =
  getExprAliases selectable
getExprAliases (SubqueryE s) =
  getQueryAliases s
getExprAliases _ =
  []

getSelectFieldAliases :: SelectField -> AliasTable
getSelectFieldAliases (SelectField _ Nothing) = []
getSelectFieldAliases (SelectField selectable (Just alias)) =
  [(alias, selectable)] <> getExprAliases selectable

{- HLINT ignore "Use record patterns" -}

getExprType :: AliasTable -> Expr -> Either String Ty
getExprType _ TrueE = return (MonoTy SqlBoolT)
getExprType _ FalseE = return (MonoTy SqlBoolT)
getExprType _ NullE = return NullTy
getExprType _ (IsNullE _) = return (MonoTy SqlBoolT)
getExprType _ (BinopE _ _ _) =
  return (MonoTy SqlBoolT)
getExprType _ (NotE _) =
  return (MonoTy SqlBoolT)
getExprType _ (AllE _) =
  return (MonoTy SqlBoolT)
getExprType _ (AnyE _) =
  return (MonoTy SqlBoolT)
getExprType _ (ColumnE colref) =
  return $ ColumnTyRef colref
getExprType aliases (AliasE a) = do
  case Map.lookup a aliases of
    Nothing ->
      Left $ "Alias does not resolve: " ++ show a
    Just selectable ->
      getExprType aliases selectable
getExprType _ (LitE v) =
  return $ getLitType v
getExprType _ (ParamE p) =
  return $ ParamTyRef p
getExprType aliases (SubqueryE s) = do
  subResults <- getQueryResultType aliases s
  case subResults of
    [ty] -> return ty
    _ -> Left $ "Incorrect subquery result type, subquery must return exactly one column"

getQueryResultType :: AliasTable -> SelectQuery -> Either String (Vector Ty)
getQueryResultType aliases q =
  case selectFields q of
    SelectFields fields -> do
      Vector.forM fields $ getExprType aliases . selectFieldSelectable

getValueType :: SqlValue -> Ty
getValueType (SqlInt _) = MonoTy SqlIntT
getValueType (SqlText _) = MonoTy SqlTextT
getValueType (SqlBytes _) = MonoTy SqlBytesT

getLitType :: Text -> Ty
getLitType t
  | Text.all isDigit t
  = MonoTy SqlIntT
  | otherwise
  = MonoTy SqlTextT

getQueryConstraints :: AliasTable -> SelectQuery -> Either String (Vector QueryConstraint)
getQueryConstraints aliases q = do
  tyConstraints <- getTypeConstraints aliases q
  return $ mconcat
    [ Vector.map TableExists . Vector.fromList . Set.toList $ getRequiredTables q
    , Vector.map ColumnExists . Vector.fromList . Set.toList $ getRequiredColumns q
    , tyConstraints
    ]

getTypeConstraints :: AliasTable -> SelectQuery -> Either String (Vector QueryConstraint)
getTypeConstraints aliases q = do
  fromConstrs <- case selectFromSource (selectFrom q) of
                      SelectFromSubquery s ->
                        getTypeConstraints aliases s
                      _ ->
                        return []
  fieldConstrs <- case selectFields q of
    SelectFields fields ->
      Vector.foldl (<>) [] <$>
        Vector.mapM (getExprTypeConstraints aliases . selectFieldSelectable) fields
  whereConstrs <- getExprTypeConstraints aliases $ selectWhere q
  return $ fromConstrs <> fieldConstrs <> whereConstrs

getExprTypeConstraints :: AliasTable -> Expr -> Either String (Vector QueryConstraint)
getExprTypeConstraints aliases (BinopE op a b) = do
  subConstraints <-
      (<>) <$> getExprTypeConstraints aliases a
           <*> getExprTypeConstraints aliases b
  (aTy, bTy) <- (,) <$> getExprType aliases a <*> getExprType aliases b
  immediateConstraints <-
    case op of
      Equals -> return [CompatibleTypes aTy bTy]
      NotEquals -> return [CompatibleTypes aTy bTy]
      Less -> return [CompatibleTypes aTy bTy]
      Greater -> return [CompatibleTypes aTy bTy]
  return $ subConstraints <> immediateConstraints
getExprTypeConstraints aliases (NotE w) =
  getExprTypeConstraints aliases w
getExprTypeConstraints aliases (AllE ws) =
  Vector.foldl (<>) [] <$> Vector.mapM (getExprTypeConstraints aliases) ws
getExprTypeConstraints aliases (AnyE ws) =
  Vector.foldl (<>) [] <$> Vector.mapM (getExprTypeConstraints aliases) ws
getExprTypeConstraints aliases (SubqueryE s) =
  getQueryConstraints aliases s
getExprTypeConstraints _ _ =
  return []

getRequiredTables :: SelectQuery -> Set TableName
getRequiredTables q =
  let requiredFrom = case selectFromSource (selectFrom q) of
        SelectFromDual -> []
        SelectFromTable n -> [n]
        SelectFromSubquery s -> getRequiredTables s
      requiredFields = getFieldsRequiredTables $ selectFields q
      requiredWhere = getExprRequiredTables $ selectWhere q
  in requiredFrom <> requiredFields <> requiredWhere

getFieldsRequiredTables :: SelectFields -> Set TableName
getFieldsRequiredTables (SelectFields fields) =
  Vector.foldl (<>) [] $ Vector.map getFieldRequiredTables fields

getExprRequiredTables :: Expr -> Set TableName
getExprRequiredTables (BinopE _ a b) =
  getExprRequiredTables a <> getExprRequiredTables b
getExprRequiredTables (AllE ws) =
  Vector.foldl (<>) [] $ Vector.map getExprRequiredTables ws
getExprRequiredTables (AnyE ws) =
  Vector.foldl (<>) [] $ Vector.map getExprRequiredTables ws
getExprRequiredTables (IsNullE w) =
  getExprRequiredTables w
getExprRequiredTables (NotE w) =
  getExprRequiredTables w
getExprRequiredTables (ColumnE (ColumnRef tn _)) =
  [tn]
getExprRequiredTables (SubqueryE s) =
  getRequiredTables s
getExprRequiredTables _ =
  []

getFieldRequiredTables :: SelectField -> Set TableName
getFieldRequiredTables field =
  getExprRequiredTables (selectFieldSelectable field)

getRequiredColumns :: SelectQuery -> Set ColumnRef
getRequiredColumns q =
  let fromColumns = case selectFromSource (selectFrom q) of
        SelectFromSubquery s -> getRequiredColumns s
        _ -> []
      fromFields = case selectFields q of
        SelectFields fields -> Vector.foldl (<>) [] $ Vector.map getFieldRequiredColumns fields
      fromWhere = getExprRequiredColumns $ selectWhere q
  in fromColumns <> fromFields <> fromWhere

getFieldRequiredColumns :: SelectField -> Set ColumnRef
getFieldRequiredColumns field =
  getExprRequiredColumns $ selectFieldSelectable field

getExprRequiredColumns :: Expr -> Set ColumnRef
getExprRequiredColumns (BinopE _ a b) =
  getExprRequiredColumns a <> getExprRequiredColumns b
getExprRequiredColumns (AllE ws) =
  Vector.foldl (<>) [] $ Vector.map getExprRequiredColumns ws
getExprRequiredColumns (AnyE ws) =
  Vector.foldl (<>) [] $ Vector.map getExprRequiredColumns ws
getExprRequiredColumns (NotE w) =
  getExprRequiredColumns w
getExprRequiredColumns (ColumnE colref) =
  [colref]
getExprRequiredColumns (SubqueryE s) =
  getRequiredColumns s
getExprRequiredColumns _ =
  []
