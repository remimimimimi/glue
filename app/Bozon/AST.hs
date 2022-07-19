{-# LANGUAGE OverloadedStrings #-}

module Bozon.AST where

import Data.Text
import Numeric.Natural
import Text.Megaparsec.Pos

data Span = Span Natural Natural
  deriving (Show, Eq)

data Node a = Node a Span
  deriving (Show, Eq)

instance Functor Node where
  fmap f (Node a s) = Node (f a) s

data BracketKind = Circle | Curly | Square
  deriving (Show, Eq)

data Sexp
  = -- | List of sexps with optinal prefix and bracket type
    List (Maybe (Node Text)) BracketKind [Node Sexp]
  | -- | Single atom
    Atom Text
  deriving (Show, Eq)

data File = File (Node Text) [Node Sexp]
  deriving (Show, Eq)
