{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Posterchild.Parser.Select
where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Vector as Vector
import Data.Text (Text)
import qualified Data.Text as Text

import Database.Posterchild.Syntax.Common
import Database.Posterchild.Syntax.Abstract
import Database.Posterchild.Parser.Common
import Database.Posterchild.Parser.Expr

parseSelect :: MonadFail m => String -> String -> m SelectQuery
parseSelect filename input =
  case parse (whitespaceP *> selectP) filename input of
    Right e -> return e
    Left err -> fail $ errorBundlePretty err

selectP :: Parser s SelectQuery
selectP =
  label "SELECT query" $ do
    fields <- string' "SELECT" *> whitespaceP *> selectFieldsP <* whitespaceP
    from <- string' "FROM" *> whitespaceP *> selectFromP <* whitespaceP
    where_ <- option TrueE $ string' "WHERE" *> whitespaceP *> sqlExprP <* whitespaceP
    return $ SelectQuery from fields where_

selectFieldsP :: Parser s SelectFields
selectFieldsP = SelectFields . Vector.fromList <$>
  sepBy selectFieldP (string "," *> whitespaceP)

selectFieldP :: Parser s SelectField
selectFieldP = label "SELECT column" $ do
  selectable <- subqueryExprP <|> sqlExprP
  aliasMay <- fmap ColumnName <$> aliasP
  return $ SelectField selectable aliasMay

subqueryExprP :: Parser s Expr
subqueryExprP = SubqueryE <$> subqueryP

selectFromP :: Parser s SelectFrom
selectFromP = do
  source <- selectSourceP
  aliasMay <- fmap TableName <$> aliasP
  alias <- case (source, aliasMay) of
    (_, Just alias) -> return alias
    (SelectFromTable tn, Nothing) -> return tn
    (_, _) -> fail $ "Alias required for select source: " ++ show source
  return $ SelectFrom source alias

selectSourceP :: Parser s SelectSource
selectSourceP = label "selectable" $ do
  simpleSourceP
  -- lhs <- simpleSourceP
  -- option lhs $ do
  --   joinType <- joinTypeP
  --   rhs <- selectSourceP
  --   void $ string' "ON" <* whitespaceP
  --   cond <- sqlExprP
  --   return $ Join joinType lhs rhs cond

simpleSourceP :: Parser s SelectSource
simpleSourceP =
  (SelectFromSubquery <$> subqueryP) <|>
  (SelectFromTable <$> ((TableName <$> identifierP) <* whitespaceP))

subqueryP :: Parser s SelectQuery
subqueryP =
  label "subquery" $
    (char '(' *> whitespaceP *> selectP <* char ')' <* whitespaceP)

aliasP :: Parser s (Maybe Text)
aliasP = optional $ do
  keywordP "as"
  whitespaceP *> identifierP <* whitespaceP

-- joinTypeP :: Parser s JoinType
-- joinTypeP = label "join type" $
--   choice
--     [ InnerJoin <$ (keywordP "INNER" *> keywordP "JOIN")
--     , LeftOuterJoin <$ (keywordP "LEFT" *> optional (keywordP "OUTER") *> keywordP "JOIN")
--     , RightOuterJoin <$ (keywordP "RIGHT" *> optional (keywordP "OUTER") *> keywordP "JOIN")
--     , FullOuterJoin <$ (keywordP "FULL" *> keywordP "OUTER" *> keywordP "JOIN")
--     ] <* whitespaceP
