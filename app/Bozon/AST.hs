{-# LANGUAGE OverloadedStrings #-}

module Bozon.AST (Span (..), Node (..), Sexp (..), File (..)) where

import Data.Text
import Numeric.Natural
import Text.Megaparsec.Pos

data Span = Span Natural Natural
  deriving (Show, Eq)

data Node a = Node a Span
  deriving (Show, Eq)

instance Functor Node where
  fmap f (Node a s) = Node (f a) s

data Sexp
  = -- | List of sexps with optinal prefix
    List (Maybe (Node Text)) [Node Sexp]
  | -- | Single atom
    Atom Text
  deriving (Show, Eq)

data File = File (Node Text) [Node Sexp]
  deriving (Show, Eq)
