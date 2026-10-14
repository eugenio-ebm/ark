module Ark.Expr (
  Expr(..)
  ) where

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Int Int
  deriving (Eq, Show)
