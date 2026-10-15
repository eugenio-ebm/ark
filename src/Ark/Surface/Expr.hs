module Ark.Surface.Expr ( Expr(..), BinOp(..) ) where

data Expr
  = BinOp BinOp Expr Expr
  | Int Int
  deriving (Eq, Show)

data BinOp
  = Add
  | Sub
  deriving (Eq, Show)
