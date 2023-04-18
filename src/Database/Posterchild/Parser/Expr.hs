{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Posterchild.Parser.Expr
where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Vector as Vector
import Data.Text (Text)
import qualified Data.Text as Text
import Data.String (fromString)

import Database.Posterchild.Syntax.Common
import Database.Posterchild.Syntax.Abstract
import Database.Posterchild.Parser.Common

sqlExprP :: Parser s Expr
sqlExprP = label "expression" $ do
  notFollowedBy (keywordP "as")
  notFollowedBy (keywordP "where")
  andExprP

andExprP :: Parser s Expr
andExprP = do
  lhs <- orExprP
  tails <- many $
    keywordP "and" *> orExprP
  case tails of
    [] -> return lhs
    _ -> return $ FoldE All $ Vector.fromList (lhs : tails)

orExprP :: Parser s Expr
orExprP = do
  lhs <- compExprP
  tails <- many $
    keywordP "or" *> orExprP
  case tails of
    [] -> return lhs
    _ -> return $ FoldE Any $ Vector.fromList (lhs : tails)

compExprP :: Parser s Expr
compExprP = label "comparative expression" $ do
  lhs <- appExprP
  opMay <- optional binopP
  case opMay of
    Just op -> do
      rhs <- sqlExprP
      return $ BinopE op lhs rhs
    Nothing ->
      return lhs

binopP :: Parser s Binop
binopP =
  label "binary operator" $ choice
    [ Equals <$ string "="
    , NotEquals <$ string "<>"
    , Less <$ string "<"
    , Greater <$ string ">"
    ]
    <* whitespaceP

appExprP :: Parser s Expr
appExprP = typedExprP
-- TODO: implement below
-- appExprP = do
--   lhs <- typedExprP
--   tails <- many $ parenthesizedP (sqlExprP `sepBy` commaP)
--   return $ foldl SqlAppE lhs tails

typedExprP :: Parser s Expr
typedExprP = simpleExprP
-- TODO: implement below
-- typedExprP = do
--   val <- simpleExprP
--   tyTailMay <- optional $ string "::" *> sqlBaseTypeP
--   case tyTailMay of
--     Nothing -> return val
--     Just tyTail -> return $ SqlTyAnnE tyTail val

simpleExprP :: Parser s Expr
simpleExprP =
  nullExprP <|> paramExprP <|> numLitExprP <|> litExprP <|> refExprP

nullExprP :: Parser s Expr
nullExprP = NullE <$ keywordP "null" <?> "null"

paramExprP :: Parser s Expr
paramExprP = ParamE . ParamName . Text.pack <$> (char '$' *> uintLitP <* whitespaceP) <?> "parameter"

litExprP :: Parser s Expr
litExprP = do
  LitE . fromString <$> (char '\'' *> many litCharP <* char '\'' <* whitespaceP) <?> "quoted literal"

litCharP :: Parser s Char
litCharP =
  ('\'' <$ string "''") <|>
  satisfy (/= '\'')

numLitExprP :: Parser s Expr
numLitExprP = LitE <$> numLitP <* whitespaceP <?> "number literal"

refExprP :: Parser s Expr
refExprP = label "column reference" $ do
  parts <- identifierPartsP
  case parts of
    [] -> fail "This won't happen"
    [name] -> return $ RefE Nothing (ColumnName name)
    [ns,name] -> return $ RefE (Just $ TableName ns) (ColumnName name)
    xs -> fail $ "Name too long: " ++ show xs

