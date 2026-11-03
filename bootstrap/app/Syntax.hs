module Syntax (
  Id,
  Expr(..),
  Pat(..),
  BinOp(..),
  Val(..),
  ) where

type Id = String

data Expr
  = Let Id Expr Expr
  | Lam Id Expr
  | Match Expr [(Pat, Expr)]
  | BinOp BinOp Expr Expr
  | Cons Id [Expr]
  | App Expr Expr
  | Var Id
  | Str String
  | Int Int
  | Unit
  deriving (Eq, Show)


data Pat
  = PCons Id [Id]
  deriving (Eq, Show)
  
data BinOp
  = Add
  | Sub
  | Mul
  | Lt
  | Append
  deriving (Eq, Show)

data Val
  = VStr String
  | VInt Int
  | VPrim String
  | VCons Id [Val]
  | VClo Id Expr [(Id, Val)]
  | VUnit
  deriving (Eq)

instance Show Val where
  show (VStr s) = s
  show (VInt v) = show v
  show (VPrim p) = "<" ++ p ++ ">"
  show (VClo _ _ _) = "<closure>"
  show (VCons f args) = "(" ++ f ++ concatMap ((" " ++) . show) args ++ ")"
  show VUnit = "()"

