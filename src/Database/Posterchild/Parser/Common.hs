{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Database.Posterchild.Parser.Common
where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad
import Data.Char
import Data.String
import Data.Void
import Data.CaseInsensitive (FoldCase)
import Data.Text (Text)
import qualified Data.Text as Text

type Parser s a = (Stream s, Show (Tokens s), IsString (Tokens s), Token s ~ Char, FoldCase (Tokens s))
    => Parsec Void s a

identifierP :: Parser s Text
identifierP = Text.intercalate "." <$> identifierPartsP

identifierPartsP :: Parser s [Text]
identifierPartsP = (identifierPartP `sepBy` char '.') <* whitespaceP

identifierPartP :: Parser s Text
identifierPartP = quotedIdentifierP <|> barewordP <?> "identifier"

quotedIdentifierP :: Parser s Text
quotedIdentifierP =
  fmap fromString
    (char '"' *> many quotedIdentifierCharP <* char '"')

quotedIdentifierCharP :: Parser s Char
quotedIdentifierCharP =
  ('"' <$ string "\"\"") <|>
  satisfy (/= '"')

lineCommentP :: Parser s ()
lineCommentP = L.skipLineComment "--" <* eol

whitespaceP :: Parser s ()
whitespaceP = void $ many
  (lineCommentP <|> space1)

keywordP :: Tokens s -> Parser s ()
keywordP kw =
  label ("keyword " ++ show kw) $
  void $ string' kw <* notFollowedBy (satisfy isAlphaNum) <* whitespaceP

keywordsP :: [Tokens s] -> Parser s ()
keywordsP = mapM_ keywordP

parenthesizedP :: Parser s a -> Parser s a
parenthesizedP =
  label "parentheses" .
  between (char '(' *> whitespaceP) (char ')' <* whitespaceP)

bracketedP :: Parser s a -> Parser s a
bracketedP =
  label "square brackets" .
  between (char '[' *> whitespaceP) (char ']' <* whitespaceP)

commaP :: Parser s ()
commaP = label "comma" $ char ',' *> whitespaceP

semicolonP :: Parser s ()
semicolonP = label "semicolon" $ char ';' *> whitespaceP

barewordP :: Parser s Text
barewordP = label "bareword" $ do
  notFollowedBy (keywordP "as")
  notFollowedBy (keywordP "where")
  fmap fromString $ (:) <$> satisfy isAlpha <*> many (satisfy isBarewordChar) <* whitespaceP

isBarewordChar :: Char -> Bool
isBarewordChar '_' = True
isBarewordChar c = isAlphaNum c

uintLitP :: Parser s String
uintLitP = label "uint" $ do
  some (satisfy isDigit) <* whitespaceP

numLitP :: Parser s Text
numLitP = label "number" $ do
  sign <- option "" $ (:[]) <$> char '-'
  intPart <- some (satisfy isDigit)
  fracPart <- option "" $ do
    (:) <$> char '.' <*> many (satisfy isDigit)
  return . fromString $ sign <> intPart <> fracPart
