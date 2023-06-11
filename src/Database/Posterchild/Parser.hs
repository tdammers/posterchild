module Database.Posterchild.Parser
( parseSelect
)
where

import Database.Posterchild.Syntax
import PostgresqlSyntax.Ast as PG
import PostgresqlSyntax.Parsing as PGParser
import Data.List.NonEmpty

parseSelect :: String -> Text -> Either String SelectQuery
parseSelect _sourceName input =
  PGParser.run PGParser.selectStatement input >>= selectFromPG

selectFromPG :: PG.SelectQuery -> Either String SelectQuery
selectFromPG (PG.SelectNoParens s) = selectFromPGNP s
selectFromPG (PG.SelectWithParens swp) = selectFromPG swp

selectFromPGNP :: PG.SelectNoParens -> Either String SelectQuery
selectFromPGNP (PG.SelectNoParens _with clause _msort _mlimit _mforLocking) =
  case clause of
    Left s -> selectFromPGSimpleSelect
    Right swp -> selectFromPG swp

selectFromPGSimpleSelect :: PG.SimpleSelect -> Either String SelectQuery
selectFromPGSimpleSelect (PG.TableSimpleSelect {}) =
  Left "Unsupported: TableSimpleSelect"
selectFromPGSimpleSelect (PG.ValuesSimpleSelect {}) =
  Left "Unsupported: ValuesSimpleSelect"
selectFromPGSimpleSelect (PG.BinSimpleSelect {}) =
  Left "Unsupported: BinSimpleSelect"
selectFromPGSimpleSelect (PG.NormalSimpleSelect mTargeting _mInto mFrom mWhere _mGroup _mHaving _mWindow) = do
  from <- maybe (return $ SelectFromSingle DualTabloid Nothing) fromFromPGFrom mFrom
  fields <- maybe (Left "Unsupported: SELECT without targets") fieldsFromPGTargeting mTargeting
  where_ <- maybe (return $ BoolLitE True) exprFromPGAExpr mWhere
  return SelectQuery
    { selectFrom = from
    , selectFields = fields
    , selectWhere = where_
    }

fromFromPGFrom :: PG.FromClause -> Either String SelectFrom
fromFromPGFrom (x :| xs) = do
  t <- fromFromPGTableRef x
  ts <- map fromFromPGTableRef xs
  return $ foldl' combine t ts
  where
    combine a b = SelectJoin CrossJoin a b (BoolLitE True)

fieldsFromPGTargeting :: PG.Targeting -> Either String (Vector SelectField)
fieldsFromPGTargeting (PG.NormalTargeting targets) =
  fieldsFromTargetList targets
fieldsFromPGTargeting PG.AllTargeting {} =
  Left "Unsupported: SELECT *"
fieldsFromPGTargeting (PG.DistinctTargeting Nothing targets) =
  fieldsFromTargetList targets
fieldsFromPGTargeting PG.DistinctTargeting {} =
  Left "Unsupported: SELECT DISTINCT"

exprFromPGAExpr :: PG.AExpr -> Either String Expr
exprFromPGAExpr (PG.CExprAExpr cexpr) =
  exprFromPGCExpr cexpr
exprFromPGAExpr (PG.TypecastAExpr _aExpr _typename) =
  Left "Unsupported: typecasts"
exprFromPGAExpr (PG.CollateAExpr _aExpr _anyName) =
  Left "Unsupported: COLLATE"
exprFromPGAExpr (PG.AtTimeZoneAExpr _aExpr _aExpr) =
  Left "Unsupported: AT TIME ZONE"
exprFromPGAExpr (PG.PlusAExpr aExpr) =
exprFromPGAExpr (PG.MinusAExpr AExpr
exprFromPGAExpr (PG.SymbolicBinOpAExpr AExpr SymbolicExprBinOp AExpr
exprFromPGAExpr (PG.PrefixQualOpAExpr QualOp AExpr
exprFromPGAExpr (PG.SuffixQualOpAExpr AExpr QualOp
exprFromPGAExpr (PG.AndAExpr AExpr AExpr
exprFromPGAExpr (PG.OrAExpr AExpr AExpr
exprFromPGAExpr (PG.NotAExpr AExpr
exprFromPGAExpr (PG.VerbalExprBinOpAExpr AExpr Bool VerbalExprBinOp AExpr (Maybe AExpr)
exprFromPGAExpr (PG.ReversableOpAExpr AExpr Bool AExprReversableOp
exprFromPGAExpr (PG.IsnullAExpr AExpr
exprFromPGAExpr (PG.NotnullAExpr AExpr
exprFromPGAExpr (PG.OverlapsAExpr Row Row
exprFromPGAExpr (PG.SubqueryAExpr AExpr SubqueryOp SubType (Either SelectWithParens AExpr)
exprFromPGAExpr (PG.UniqueAExpr SelectWithParens
exprFromPGAExpr (PG.DefaultAExpr
