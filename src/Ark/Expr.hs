module Ark.Expr (
  Expr(..),
  BinOp(..),
  prettyExpr
  ) where

data Expr
  = BinOp BinOp Expr Expr
  | Int Int
  deriving (Eq, Show)

data BinOp
  = Add
  | Sub
  deriving (Eq, Show)

prettyExpr :: Expr -> String
prettyExpr (BinOp Add lhs rhs) =
  prettyExpr lhs ++ " + " ++ prettyExpr rhs
prettyExpr (BinOp Sub lhs rhs) =
  prettyExpr lhs ++ " - " ++ prettyExpr rhs
prettyExpr (Int v)
  | v >= 0 = show v
  | otherwise = "(" ++ show v ++ ")"
