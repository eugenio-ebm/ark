module Syntax (
  Id,
  Expr(..),
  BinOp(..),
  Val(..),
  ) where

type Id = String

data Expr
  = Let Id Expr Expr
  | Lam Id Expr
  | BinOp BinOp Expr Expr
  | App Expr Expr
  | Var Id
  | Str String
  | Int Int
  | Bool Bool
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
  | VBool Bool
  | VPrim String
  | VClo String Expr [(Id, Val)]
  deriving (Eq)

instance Show Val where
  show (VStr s) = s
  show (VInt v) = show v
  show (VBool True) = "true"
  show (VBool False) = "false"
  show (VPrim p) = "<" ++ p ++ ">"
  show (VClo _ _ _) = "<closure>"

