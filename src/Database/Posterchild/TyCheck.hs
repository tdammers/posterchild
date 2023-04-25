{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Database.Posterchild.TyCheck
where

import Control.Applicative ( (<|>) )
import Control.Monad (forM_)
import Control.Monad.Except
import Control.Monad.State
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Database.Posterchild.Syntax.Abstract
import Database.Posterchild.Syntax.SqlTy

data QueryConstraint
  = TableExists !TableName
  | ColumnExists !ColumnRef
  | SubtypeOf !Ty !Ty
  | ComparableTypes !Ty !Ty
  deriving (Show, Read, Eq)

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
    , tceConstraints :: [QueryConstraint]
    }

emptyTCEnv :: TCEnv
emptyTCEnv =
  TCEnv
    { tceTableAliases = []
    , tceColumnAliases = []
    , tceConstraints = []
    }

data TypeError
  = TypeErrorOther Text
  | TypeErrorSubqueryColumnCount
  | TypeErrorNotImplemented
    -- ^ A subquery has the wrong number of columns. This happens when a
    -- subquery is used in a column context, but returns no columns, or more
    -- than one.
  deriving (Show, Read, Eq)

type TC = ExceptT TypeError (State TCEnv)

runTC :: TC a -> Either TypeError a
runTC action =
  evalState (runExceptT action) emptyTCEnv

data SelectQueryTy =
  SelectQueryTy
    { selectQueryParamsTy :: [(ParamName, Ty)]
    , selectQueryResultTy :: [(ColumnName, Ty)]
    , selectQueryConstraintsTy :: [QueryConstraint]
    }
  deriving (Show, Read, Eq)

tcSelectQuery :: SelectQuery -> TC SelectQueryTy
tcSelectQuery q = do
  getTableAliases (selectFrom q)
  getColumnAliases q
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

cullConstraints :: [QueryConstraint] -> [QueryConstraint]
cullConstraints = filter (not . isRedundantConstraint)

isRedundantConstraint :: QueryConstraint -> Bool
isRedundantConstraint (MonoTy a `SubtypeOf` MonoTy b)
  = a `isSubtypeOf` b
isRedundantConstraint _ = False

getResultTy :: SelectQuery -> TC [(ColumnName, Ty)]
getResultTy q = do
  Vector.toList <$> Vector.mapM getResultColumn (selectFields q)

getResultColumn :: SelectField -> TC (ColumnName, Ty)
getResultColumn (SelectField expr aliasMay) = do
  let name = fromMaybe "?" $ exprToColumnName expr <|> aliasMay
  ty <- getExprTy expr
  return (name, ty)

exprToColumnName :: Expr -> Maybe ColumnName
exprToColumnName (RefE Nothing cname) = Just cname
exprToColumnName (RefE (Just (TableName tname)) (ColumnName cname)) =
  Just (ColumnName $ tname <> "." <> cname)
exprToColumnName (ParamE (ParamName pname)) =
  Just $ ColumnName pname
exprToColumnName _ = Nothing

getExprTy :: Expr -> TC Ty
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
getExprTy (RefE (Just tname) cname) =
  return $ ColumnRefTy tname cname
getExprTy (RefE Nothing _cname) =
  throwError TypeErrorNotImplemented
getExprTy (ParamE pname) =
  return $ ParamRefTy pname
getExprTy (UnopE Not e) = do
  exprTy <- getExprTy e
  addConstraint $ MonoTy SqlBooleanT `SubtypeOf` exprTy
  return $ MonoTy SqlBooleanT
getExprTy (BinopE op a b)
  | op `elem` ([Equals, NotEquals, Less, Greater] :: [Binop])
  = do
      aTy <- getExprTy a
      bTy <- getExprTy b
      addConstraint $ ComparableTypes aTy bTy
      return $ MonoTy SqlBooleanT
  | otherwise
  = throwError TypeErrorNotImplemented
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

withLocalScope :: TC a -> TC a
withLocalScope action = do
  s <- get
  action <* modify (\s' -> s { tceConstraints = tceConstraints s' })

addTableAlias :: TableName -> Tabloid -> TC ()
addTableAlias a tb =
  modify $ \s -> s { tceTableAliases = Map.insert a tb $ tceTableAliases s }

addColumnAlias :: ColumnName -> Expr -> TC ()
addColumnAlias a expr =
  modify $ \s -> s { tceColumnAliases = Map.insert a expr $ tceColumnAliases s }

addConstraint :: QueryConstraint -> TC ()
addConstraint c =
  modify $ \s -> s { tceConstraints = c : tceConstraints s }

getTableAliases :: SelectFrom -> TC ()
getTableAliases from =
  case from of
    SelectFromSingle t (Just alias) -> addTableAlias alias t
    SelectFromSingle _ _ -> return ()
    SelectJoin _ a b _ -> getTableAliases a >> getTableAliases b

getColumnAliases :: SelectQuery -> TC ()
getColumnAliases q =
  Vector.mapM_ getFieldAliases $ selectFields q
  where
    getFieldAliases :: SelectField -> TC ()
    getFieldAliases (SelectField expr (Just alias)) = addColumnAlias alias expr
    getFieldAliases _ = return ()

getParamNames :: SelectQuery -> TC [ParamName]
getParamNames q = do
  pnamesFrom <- getParamNamesInFrom (selectFrom q)
  pnamesFields <- mconcat . Vector.toList <$> Vector.mapM getParamNamesInField (selectFields q)
  pnamesWhere <- getParamNamesInExpr (selectWhere q)
  return $ pnamesFrom <> pnamesFields <> pnamesWhere

getParamNamesInFrom :: SelectFrom -> TC [ParamName]
getParamNamesInFrom (SelectFromSingle t _) =
  getParamNamesInTabloid t
getParamNamesInFrom (SelectJoin _ lhs rhs cond) = do
  mconcat <$> sequence
    [ getParamNamesInFrom lhs
    , getParamNamesInFrom rhs
    , getParamNamesInExpr cond
    ]

getParamNamesInTabloid :: Tabloid -> TC [ParamName]
getParamNamesInTabloid (SubqueryTabloid q) =
  getParamNames q
getParamNamesInTabloid _ =
  return []

getParamNamesInField :: SelectField -> TC [ParamName]
getParamNamesInField (SelectField expr _) =
  getParamNamesInExpr expr

getParamNamesInExpr :: Expr -> TC [ParamName]
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
