{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Posterchild.Parser.Select
where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad (void)

import Database.Posterchild.Parser.Common
import Database.Posterchild.Parser.Expr
import Database.Posterchild.Syntax.Abstract

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
    where_ <- option (BoolLitE True) $ string' "WHERE" *> whitespaceP *> sqlExprP <* whitespaceP
    return $ SelectQuery from fields where_

selectFieldsP :: Parser s (Vector SelectField)
selectFieldsP = Vector.fromList <$>
  sepBy selectFieldP (string "," *> whitespaceP)

selectFieldP :: Parser s SelectField
selectFieldP = label "SELECT column" $ do
  selectable <- subqueryExprP <|> sqlExprP
  aliasMay <- fmap ColumnName <$> aliasP
  return $ SelectField selectable aliasMay

subqueryExprP :: Parser s Expr
subqueryExprP = SubqueryE <$> subqueryP

selectFromP :: Parser s SelectFrom
selectFromP = label "from" $ do
  lhs <- simpleSourceP
  tails <- many joinTail
  return $ foldr ($) lhs tails
  where
    joinTail :: Parser s (SelectFrom -> SelectFrom)
    joinTail = do
      joinType <- joinTypeP
      rhs <- simpleSourceP
      void $ string' "ON"
      whitespaceP
      cond <- sqlExprP
      return $ \lhs ->
        SelectJoin joinType lhs rhs cond

simpleSourceP :: Parser s SelectFrom
simpleSourceP = do
  source <- tabloidP
  aliasMay <- fmap TableName <$> aliasP
  return $ SelectFromSingle source aliasMay

tabloidP :: Parser s Tabloid
tabloidP = label "tabloid" $
  (SubqueryTabloid <$> subqueryP) <|>
  (TableTabloid <$> ((TableName <$> identifierP) <* whitespaceP))

subqueryP :: Parser s SelectQuery
subqueryP =
  label "subquery" $
    char '(' *> whitespaceP *> selectP <* char ')' <* whitespaceP

aliasP :: Parser s (Maybe Text)
aliasP = optional $ do
  keywordP "as"
  whitespaceP *> identifierP <* whitespaceP

joinTypeP :: Parser s JoinType
joinTypeP = label "join type" $
  choice
    [ InnerJoin <$ (keywordP "INNER" *> keywordP "JOIN")
    , LeftOuterJoin <$ (keywordP "LEFT" *> optional (keywordP "OUTER") *> keywordP "JOIN")
    , RightOuterJoin <$ (keywordP "RIGHT" *> optional (keywordP "OUTER") *> keywordP "JOIN")
    , FullOuterJoin <$ (keywordP "FULL" *> keywordP "OUTER" *> keywordP "JOIN")
    ] <* whitespaceP
