{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Database.Posterchild.TyCheck
where

import Control.Applicative ( (<|>) )
import Control.Monad (forM_, void)
import Control.Monad.Except
import Control.Monad.State
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Vector as Vector
import GHC.Stack (HasCallStack)

import Database.Posterchild.Syntax

data QueryConstraint
  = TableExists !TableName
  | ColumnExists !ColumnRef
  | SubtypeOf !Ty !Ty
  | EqTypes !Ty !Ty
  | ComparableTypes !Ty !Ty
  deriving (Show, Read, Ord)

instance Eq QueryConstraint where
  x == y = case (x, y) of
    (TableExists a, TableExists b) -> a == b
    (ColumnExists a, ColumnExists b) -> a == b
    (SubtypeOf a b, SubtypeOf c d) -> a == c && b == d
    (EqTypes a b, EqTypes c d) ->
      (a == c && b == d) ||
      (a == d && b == c)
    (ComparableTypes a b, ComparableTypes c d) ->
      (a == b && b == d) ||
      (a == d && b == c)
    _ -> False


data Ty
  = NullTy
    -- ^ The NULL type.
  | MonoTy SqlTy
    -- ^ A fully resolved monomorphic type
  | ColumnRefTy !TableName !ColumnName
    -- ^ A column reference that has been unambiguously resolved to a table
    -- column
  | ParamRefTy !ParamName
    -- ^ A parameter reference
  deriving (Show, Read, Eq, Ord)

data TCEnv =
  TCEnv
    { tceTableAliases :: Map TableName Tabloid
    , tceColumnAliases :: Map ColumnName Expr
    , tceKnownColumns :: Map ColumnName (TableName, ColumnName)
    , tceKnownTables :: Set TableName
    , tceConstraints :: Set QueryConstraint
    }

emptyTCEnv :: TCEnv
emptyTCEnv =
  TCEnv
    { tceTableAliases = []
    , tceColumnAliases = []
    , tceKnownColumns = []
    , tceKnownTables = []
    , tceConstraints = []
    }

data TypeError
  = TypeErrorOther Text
  | TypeErrorSubqueryColumnCount
  | TypeErrorNotImplemented String
    -- ^ A subquery has the wrong number of columns. This happens when a
    -- subquery is used in a column context, but returns no columns, or more
    -- than one.
  | TypeErrorAmbiguousColumn ColumnName
    -- ^ An unqualified column reference could not be resolved unambiguously
  deriving (Show, Read, Eq)

type TC = ExceptT TypeError (State TCEnv)

runTC :: HasCallStack => TC a -> Either TypeError a
runTC action =
  evalState (runExceptT action) emptyTCEnv

data SelectQueryTy =
  SelectQueryTy
    { selectQueryParamsTy :: [(ParamName, Ty)]
    , selectQueryResultTy :: [(ColumnName, Ty)]
    , selectQueryConstraintsTy :: Set QueryConstraint
    }
  deriving (Show, Read, Eq)

tcSelectQuery :: HasCallStack => SelectQuery -> TC SelectQueryTy
tcSelectQuery q = do
  getKnownTables (selectFrom q)
  getTableAliases (selectFrom q)
  getColumnAliases q
  getFromConstraints (selectFrom q)
  paramNames <- getParamNames q
  let params = [ (pname, ParamRefTy pname) | pname <- paramNames ]
  resultTy <- getResultTy q
  whereTy <- getExprTy (selectWhere q)
  addConstraint $ MonoTy SqlBooleanT `SubtypeOf` whereTy
  constraints <- gets (cullConstraints . tceConstraints)

  return SelectQueryTy
    { selectQueryParamsTy = params
    , selectQueryResultTy = resultTy
    , selectQueryConstraintsTy = constraints
    }

getFromConstraints :: SelectFrom -> TC ()
getFromConstraints (SelectFromSingle tabloid _) =
  getTabloidConstraints tabloid
getFromConstraints (SelectJoin _ a b cond) = do
  getFromConstraints a
  getFromConstraints b
  void $ getExprTy cond

getTabloidConstraints :: Tabloid -> TC ()
getTabloidConstraints DualTabloid =
  return ()
getTabloidConstraints (TableTabloid tableName) =
  addConstraint $ TableExists tableName
getTabloidConstraints (SubqueryTabloid q) = do
  subqTy <- tcSelectQuery q
  addConstraints $ selectQueryConstraintsTy subqTy

cullConstraints :: Set QueryConstraint -> Set QueryConstraint
cullConstraints = Set.filter (not . isRedundantConstraint)

isRedundantConstraint :: QueryConstraint -> Bool
isRedundantConstraint (MonoTy a `SubtypeOf` MonoTy b)
  = a `isSubtypeOf` b
isRedundantConstraint _ = False

getResultTy :: HasCallStack => SelectQuery -> TC [(ColumnName, Ty)]
getResultTy q = do
  Vector.toList <$> Vector.mapM getResultColumn (selectFields q)

getResultColumn :: HasCallStack => SelectField -> TC (ColumnName, Ty)
getResultColumn (SelectField expr aliasMay) = do
  let name = fromMaybe "?" $ aliasMay <|> exprToColumnName expr
  ty <- getExprTy expr
  return (name, ty)

exprToColumnName :: HasCallStack => Expr -> Maybe ColumnName
exprToColumnName (RefE Nothing cname) = Just cname
exprToColumnName (RefE (Just (TableName tname)) (ColumnName cname)) =
  Just (ColumnName $ tname <> "." <> cname)
exprToColumnName (ParamE (ParamName pname)) =
  Just $ ColumnName pname
exprToColumnName _ = Nothing

getExprTy :: HasCallStack => Expr -> TC Ty
getExprTy NullE =
  return NullTy
getExprTy (BoolLitE _) =
  return $ MonoTy SqlBooleanT
getExprTy (IntLitE i)
  | i >= fromIntegral (minBound :: Int16)
  , i <= fromIntegral (maxBound :: Int16)
  = return $ MonoTy SqlSmallIntT
getExprTy (IntLitE i)
  | i >= fromIntegral (minBound :: Int32)
  , i <= fromIntegral (maxBound :: Int32)
  = return $ MonoTy SqlIntegerT
getExprTy (IntLitE i)
  | i >= fromIntegral (minBound :: Int64)
  , i <= fromIntegral (maxBound :: Int64)
  = return $ MonoTy SqlBigIntT
getExprTy (IntLitE _)
  = return $ MonoTy SqlBlobT
getExprTy (LitE _) =
  return $ MonoTy SqlAnyT
getExprTy (RefE (Just tname) cname) = do
  tabloid <- resolveTableName tname
  case tabloid of
    TableTabloid tname' -> do
      addConstraint $ TableExists tname'
      addConstraint $ ColumnExists (ColumnRef tname' cname)
      return $ ColumnRefTy tname' cname
    _ -> do
      return $ ColumnRefTy tname cname
getExprTy (RefE Nothing cname) = do
  knownTables <- gets tceKnownTables
  case Set.toList knownTables of
    [tname] -> do
      addConstraint $ TableExists tname
      addConstraint $ ColumnExists (ColumnRef tname cname)
      return $ ColumnRefTy tname cname
    _ ->
      throwError (TypeErrorAmbiguousColumn cname)
getExprTy (ParamE pname) =
  return $ ParamRefTy pname
getExprTy (UnopE Not e) = do
  exprTy <- getExprTy e
  addConstraint $ MonoTy SqlBooleanT `SubtypeOf` exprTy
  return $ MonoTy SqlBooleanT
getExprTy (BinopE op a b)
  | op `elem` ([Equals, NotEquals] :: [Binop])
  = do
      aTy <- getExprTy a
      bTy <- getExprTy b
      addConstraint $ EqTypes aTy bTy
      return $ MonoTy SqlBooleanT
  | op `elem` ([Less, Greater] :: [Binop])
  = do
      aTy <- getExprTy a
      bTy <- getExprTy b
      addConstraint $ ComparableTypes aTy bTy
      return $ MonoTy SqlBooleanT
  | otherwise
  = throwError (TypeErrorNotImplemented $ "Operator " ++ show op)
getExprTy (FoldE _ exprs) = do
  exprTys <- Vector.toList <$> Vector.mapM getExprTy exprs
  forM_ exprTys $ \exprTy -> do
    addConstraint $ MonoTy SqlBooleanT `SubtypeOf` exprTy
  return $ MonoTy SqlBooleanT
getExprTy (SubqueryE q) = do
  qTy <- withLocalScope $ tcSelectQuery q
  case selectQueryResultTy qTy of
    [(_, ty)] -> return ty
    _ -> throwError TypeErrorSubqueryColumnCount

withLocalScope :: HasCallStack => TC a -> TC a
withLocalScope action = do
  s <- get
  action <* modify (\s' -> s { tceConstraints = tceConstraints s' })

resolveTableName :: HasCallStack => TableName -> TC Tabloid
resolveTableName tn = do
  aliases <- gets tceTableAliases
  case Map.lookup tn aliases of
    Nothing -> return (TableTabloid tn)
    Just (TableTabloid n) -> resolveTableName n
    Just t -> return t

addKnownTable :: HasCallStack => TableName -> TC ()
addKnownTable tb =
  modify $ \s -> s { tceKnownTables = Set.insert tb $ tceKnownTables s }

addTableAlias :: HasCallStack => TableName -> Tabloid -> TC ()
addTableAlias a tb =
  modify $ \s -> s { tceTableAliases = Map.insert a tb $ tceTableAliases s }

addColumnAlias :: HasCallStack => ColumnName -> Expr -> TC ()
addColumnAlias a expr =
  modify $ \s -> s { tceColumnAliases = Map.insert a expr $ tceColumnAliases s }

addConstraint :: HasCallStack => QueryConstraint -> TC ()
addConstraint c =
  modify $ \s -> s { tceConstraints = Set.insert c (tceConstraints s) }

addConstraints :: HasCallStack => Set QueryConstraint -> TC ()
addConstraints cs =
  modify $ \s -> s { tceConstraints = Set.union cs (tceConstraints s) }

getKnownTables :: HasCallStack => SelectFrom -> TC ()
getKnownTables (SelectFromSingle (TableTabloid tname) _) =
  addKnownTable tname
getKnownTables (SelectJoin _ a b _) = do
  getKnownTables a
  getKnownTables b
getKnownTables _ =
   return ()

getTableAliases :: HasCallStack => SelectFrom -> TC ()
getTableAliases from =
  case from of
    SelectFromSingle t (Just alias) ->
      addTableAlias alias t
    SelectFromSingle _ _ ->
      return ()
    SelectJoin _ a b _ ->
      getTableAliases a >> getTableAliases b

getColumnAliases :: HasCallStack => SelectQuery -> TC ()
getColumnAliases q =
  Vector.mapM_ getFieldAliases $ selectFields q
  where
    getFieldAliases :: SelectField -> TC ()
    getFieldAliases (SelectField expr (Just alias)) = addColumnAlias alias expr
    getFieldAliases _ = return ()

getParamNames :: HasCallStack => SelectQuery -> TC [ParamName]
getParamNames q = do
  pnamesFrom <- getParamNamesInFrom (selectFrom q)
  pnamesFields <- mconcat . Vector.toList <$> Vector.mapM getParamNamesInField (selectFields q)
  pnamesWhere <- getParamNamesInExpr (selectWhere q)
  return $ pnamesFrom <> pnamesFields <> pnamesWhere

getParamNamesInFrom :: HasCallStack => SelectFrom -> TC [ParamName]
getParamNamesInFrom (SelectFromSingle t _) =
  getParamNamesInTabloid t
getParamNamesInFrom (SelectJoin _ lhs rhs cond) = do
  mconcat <$> sequence
    [ getParamNamesInFrom lhs
    , getParamNamesInFrom rhs
    , getParamNamesInExpr cond
    ]

getParamNamesInTabloid :: HasCallStack => Tabloid -> TC [ParamName]
getParamNamesInTabloid (SubqueryTabloid q) =
  getParamNames q
getParamNamesInTabloid _ =
  return []

getParamNamesInField :: HasCallStack => SelectField -> TC [ParamName]
getParamNamesInField (SelectField expr _) =
  getParamNamesInExpr expr

getParamNamesInExpr :: HasCallStack => Expr -> TC [ParamName]
getParamNamesInExpr (ParamE p) = return [p]
getParamNamesInExpr (UnopE _ e) = getParamNamesInExpr e
getParamNamesInExpr (BinopE _ a b) =
  (<>) <$> getParamNamesInExpr a <*> getParamNamesInExpr b
getParamNamesInExpr (FoldE _ exprs) =
  mconcat . Vector.toList <$> Vector.mapM getParamNamesInExpr exprs
getParamNamesInExpr (SubqueryE q) =
  getParamNames q
getParamNamesInExpr _ =
  return []
