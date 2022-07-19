{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Bozon.Parser where

import Bozon.AST
-- import Control.Applicative
import Control.Monad
import Data.Text
import Data.Void
import GHC.Real (naturalFromInt)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Aux combinators

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment ";;")
    Text.Megaparsec.empty

spanned :: Parser a -> Parser (Node a)
spanned p = do
  start <- naturalFromInt <$> getOffset
  x <- p
  end <- naturalFromInt <$> getOffset
  return $ Node x $ Span start end

pIdent :: Parser (Node Text)
pIdent =
  let banned = [' ', '\n', '\r', '(', ')', '{', '}', '[', ']']
   in spanned (pack <$> some (noneOf banned)) -- TODO: Add unicode symbols

pAtom, pList, pSexp :: Parser (Node Sexp)
pAtom = (Atom <$>) <$> pIdent
pList = spanned $ do
  prefix <- optional pIdent
  (bracketKind, list) <-
    choice
      [ (Circle,) <$> between (char '(') (char ')') (pSexp `sepBy` sc),
        (Curly,) <$> between (char '{') (char '}') (pSexp `sepBy` sc),
        (Square,) <$> between (char '[') (char ']') (pSexp `sepBy` sc)
      ]
  return $ List prefix bracketKind list
pSexp = sc *> (try pList <|> pAtom) <* sc

pLangName :: Parser (Node Text)
pLangName = do
  char '#'
  name <- pIdent
  char '\n'
  return name

pFile :: Parser File
pFile = do
  langName <- pLangName
  sExps <- pSexp `sepBy` sc
  return $ File langName sExps
