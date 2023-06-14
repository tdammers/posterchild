{-# LANGUAGE OverloadedLists #-}

module Database.Posterchild.Parser
( parseSelect
)
where

import Database.Posterchild.Syntax
import qualified PostgresqlSyntax.Ast as PG
import qualified PostgresqlSyntax.Parsing as PGParser
import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.List (foldl')
import Debug.Trace

parseSelect :: String -> Text -> Either String SelectQuery
parseSelect _sourceName input = do
  astPG <- PGParser.run PGParser.selectStmt input
  selectFromPG astPG

selectFromPG :: PG.SelectStmt -> Either String SelectQuery
selectFromPG (Left s) = selectFromPGNP s
selectFromPG (Right s) = selectFromPGWP s


selectFromPGWP :: PG.SelectWithParens -> Either String SelectQuery
selectFromPGWP (PG.NoParensSelectWithParens snp) = selectFromPGNP snp
selectFromPGWP (PG.WithParensSelectWithParens swp) = selectFromPGWP swp

selectFromPGNP :: PG.SelectNoParens -> Either String SelectQuery
selectFromPGNP (PG.SelectNoParens _with clause msorts _mlimit _mforLocking) = do
  baseSelect <- case clause of
    Left s -> selectFromPGSimpleSelect s
    Right swp -> selectFromPGWP swp
  sort <- maybe (return []) sortsFromPG msorts
  return $ baseSelect { selectSort = sort }

sortsFromPG :: PG.SortClause -> Either String (Vector SelectSort)
sortsFromPG = fmap Vector.fromList . mapM sortFromPG . NE.toList

sortFromPG :: PG.SortBy -> Either String SelectSort
sortFromPG PG.UsingSortBy {} =
  Left "Unsupported: ORDER BY ... USING"
sortFromPG (PG.AscDescSortBy exprPG mAscDescPG _mNullsOrderPG) = do
  let ascDesc = case mAscDescPG of
        Nothing -> SortAscending
        Just PG.AscAscDesc -> SortAscending
        Just PG.DescAscDesc -> SortDescending
  expr <- exprFromPGAExpr exprPG
  return $ SelectSort expr ascDesc

selectFromPGSimpleSelect :: PG.SimpleSelect -> Either String SelectQuery
selectFromPGSimpleSelect (PG.TableSimpleSelect {}) =
  Left "Unsupported: TableSimpleSelect"
selectFromPGSimpleSelect (PG.ValuesSimpleSelect {}) =
  Left "Unsupported: ValuesSimpleSelect"
selectFromPGSimpleSelect (PG.BinSimpleSelect {}) =
  Left "Unsupported: BinSimpleSelect"
selectFromPGSimpleSelect (PG.NormalSimpleSelect mTargeting mInto mFrom mWhere mGroup mHaving mWindow) = do
  from <- maybe (return $ SelectFromSingle DualTabloid Nothing) fromFromPGFrom mFrom
  fields <- maybe (return []) fieldsFromPGTargeting mTargeting
  where_ <- maybe (return $ BoolLitE True) exprFromPGAExpr mWhere
  having <- maybe (return $ BoolLitE True) exprFromPGAExpr mHaving
  case mGroup of
    Nothing -> return ()
    Just _ -> Left "Unsupported: GROUP BY"
  case mInto of
    Nothing -> return ()
    Just _ -> Left "Unsupported: SELECT INTO"
  case mWindow of
    Nothing -> return ()
    Just _ -> Left "Unsupported: WINDOW"

  return SelectQuery
    { selectFrom = from
    , selectFields = fields
    , selectWhere = where_
    , selectHaving = having
    , selectSort = []
    }

fromFromPGFrom :: PG.FromClause -> Either String SelectFrom
fromFromPGFrom (x :| xs) = do
  t <- fromFromPGTableRef x
  ts <- mapM fromFromPGTableRef xs
  return $ foldl' combine t ts
  where
    combine a b = SelectJoin CrossJoin a b (BoolLitE True)

fromFromPGTableRef :: PG.TableRef -> Either String SelectFrom
fromFromPGTableRef (PG.RelationExprTableRef relE mAlias Nothing) = do
  name <- case relE of
            PG.SimpleRelationExpr qname _ ->
              tableNameFromQName qname
            PG.OnlyRelationExpr qname _ ->
              tableNameFromQName qname
  alias <- mTableAliasFromMaybePGAlias mAlias
  return $ SelectFromSingle (TableTabloid name) alias
fromFromPGTableRef (PG.JoinTableRef joinedTable mAlias) = do
  (method, left, right, cond) <- joinFromPGJoin joinedTable
  _alias <- mTableAliasFromMaybePGAlias mAlias
  traceShowM _alias
  return $ SelectJoin method left right cond
fromFromPGTableRef (PG.SelectTableRef _lateral swp mAlias) = do
  alias <- mTableAliasFromMaybePGAlias mAlias
  sq <- selectFromPGWP swp
  return $ SelectFromSingle (SubqueryTabloid sq) alias

fromFromPGTableRef x =
  Left $ "Unsupported: " ++ show x

joinFromPGJoin :: PG.JoinedTable
               -> Either String (JoinType, SelectFrom, SelectFrom, Expr)
joinFromPGJoin (PG.InParensJoinedTable t) = joinFromPGJoin t
joinFromPGJoin (PG.MethJoinedTable methPG leftRef rightRef) = do
  left <- fromFromPGTableRef leftRef
  right <- fromFromPGTableRef rightRef
  (ty, cond) <- case methPG of
    PG.CrossJoinMeth ->
      return (CrossJoin, BoolLitE True)
    PG.NaturalJoinMeth mTy ->
      return (joinTyFromPG mTy, BoolLitE True)
    PG.QualJoinMeth mTy joinQual -> do
      joinCond <- case joinQual of
        PG.OnJoinQual joinExpr -> exprFromPGAExpr joinExpr
        x -> Left $ "Unsupported: " ++ show x
      return (joinTyFromPG mTy, joinCond)
  return (ty, left, right, cond)
  where
    joinTyFromPG :: Maybe PG.JoinType -> JoinType
    joinTyFromPG Nothing = CrossJoin
    joinTyFromPG (Just PG.FullJoinType {}) = FullOuterJoin
    joinTyFromPG (Just PG.LeftJoinType {}) = LeftOuterJoin
    joinTyFromPG (Just PG.RightJoinType {}) = RightOuterJoin
    joinTyFromPG (Just PG.InnerJoinType) = InnerJoin

mTableAliasFromMaybePGAlias :: Maybe PG.AliasClause -> Either String (Maybe TableName)
mTableAliasFromMaybePGAlias mAlias =
  maybe (return Nothing) (fmap (Just . TableName) . aliasFromAliasClause) mAlias

tableNameFromQName :: PG.QualifiedName -> Either String TableName
tableNameFromQName (PG.SimpleQualifiedName ident) =
  TableName <$> textFromIdent ident
tableNameFromQName (PG.IndirectedQualifiedName _ _) =
  Left "Unsupported: indirected names"
textFromIdent :: PG.Ident -> Either String Text
textFromIdent (PG.QuotedIdent i) = Right i
textFromIdent (PG.UnquotedIdent i) = Right i

aliasFromAliasClause :: PG.AliasClause -> Either String Text
aliasFromAliasClause (PG.AliasClause _ ident _) =
  textFromIdent ident

fieldsFromPGTargeting :: PG.Targeting -> Either String (Vector SelectField)
fieldsFromPGTargeting (PG.NormalTargeting targets) =
  fieldsFromTargetList targets
fieldsFromPGTargeting PG.AllTargeting {} =
  Left "Unsupported: SELECT *"
fieldsFromPGTargeting (PG.DistinctTargeting Nothing targets) =
  fieldsFromTargetList targets
fieldsFromPGTargeting PG.DistinctTargeting {} =
  Left "Unsupported: SELECT DISTINCT"

fieldsFromTargetList :: PG.TargetList -> Either String (Vector SelectField)
fieldsFromTargetList xs =
  Vector.fromList <$> mapM fieldFromTargetEl (NE.toList xs)

fieldFromTargetEl :: PG.TargetEl -> Either String SelectField
fieldFromTargetEl (PG.AliasedExprTargetEl expr alias) =
  SelectField <$> exprFromPGAExpr expr <*> (pure . ColumnName <$> textFromIdent alias)
fieldFromTargetEl (PG.ImplicitlyAliasedExprTargetEl expr alias) =
  SelectField <$> exprFromPGAExpr expr <*> (pure . ColumnName <$> textFromIdent alias)
fieldFromTargetEl (PG.ExprTargetEl expr) =
  SelectField <$> exprFromPGAExpr expr <*> pure Nothing
fieldFromTargetEl PG.AsteriskTargetEl =
  Left "Unsupported: SELECT *"

exprFromPGAExpr :: PG.AExpr -> Either String Expr
exprFromPGAExpr (PG.CExprAExpr cexpr) =
  exprFromPGCExpr cexpr
exprFromPGAExpr (PG.TypecastAExpr _aExpr _typename) =
  Left "Unsupported: typecasts"
exprFromPGAExpr (PG.CollateAExpr _aExpr _anyName) =
  Left "Unsupported: COLLATE"
exprFromPGAExpr (PG.AtTimeZoneAExpr _aExpr1 _aExpr2) =
  Left "Unsupported: AT TIME ZONE"
exprFromPGAExpr (PG.PlusAExpr aExpr) =
  UnopE UnaryPlus <$> exprFromPGAExpr aExpr
exprFromPGAExpr (PG.MinusAExpr aExpr) =
  UnopE UnaryMinus <$> exprFromPGAExpr aExpr
exprFromPGAExpr (PG.SymbolicBinOpAExpr lhsPG opPG rhsPG) = do
  lhs <- exprFromPGAExpr lhsPG
  rhs <- exprFromPGAExpr rhsPG
  op <- binopFromPG opPG
  return $ BinopE op lhs rhs
exprFromPGAExpr x =
  Left $ "Unsupported: " ++ show x

exprFromPGCExpr :: PG.CExpr -> Either String Expr
exprFromPGCExpr (PG.ColumnrefCExpr (PG.Columnref colID Nothing)) = do
  RefE Nothing . ColumnName <$> textFromIdent colID
exprFromPGCExpr (PG.ColumnrefCExpr (PG.Columnref tblID (Just (indirection :| [])))) = do
  case indirection of
    PG.AttrNameIndirectionEl colID ->
      RefE <$> (Just . TableName <$> textFromIdent tblID) <*> (ColumnName <$> textFromIdent colID)
    x -> Left $ "Unsupported: " ++ show x

exprFromPGCExpr (PG.AexprConstCExpr (PG.IAexprConst n)) =
  return $ IntLitE $ toInteger n

exprFromPGCExpr x =
  Left $ "Unsupported: " ++ show x

binopFromPG :: PG.SymbolicExprBinOp -> Either String Binop
binopFromPG (PG.MathSymbolicExprBinOp PG.PlusMathOp) = return Plus
binopFromPG (PG.MathSymbolicExprBinOp PG.MinusMathOp) = return Minus
binopFromPG (PG.MathSymbolicExprBinOp PG.EqualsMathOp) = return Equals
binopFromPG x = Left $ "Unsupported: " ++ show x
