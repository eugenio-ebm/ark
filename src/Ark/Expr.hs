module Ark.Expr (
  Expr(..),
  prettyExpr
  ) where

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Int Int
  deriving (Eq, Show)

prettyExpr :: Expr -> String
prettyExpr (Add lhs rhs) =
  prettyExpr lhs ++ " + " ++ prettyExpr rhs
prettyExpr (Sub lhs rhs) =
  prettyExpr lhs ++ " - " ++ prettyExpr rhs
prettyExpr (Int v)
  | v >= 0 = show v
  | otherwise = "(" ++ show v ++ ")"
