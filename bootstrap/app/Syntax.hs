module Syntax (
  Expr(..),
  BinOp(..),
  eval,
  ) where

import Control.Monad.Reader ( Reader, runReader, ask, local )
-- import Control.Monad.State ( State, evalState, get, modify )

type Id = String

data Expr
  = Let Id Expr Expr
  | BinOp BinOp Expr Expr
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
  deriving (Eq, Show)

eval :: Expr -> Val
eval expr = runReader (eval' expr) []
  where
    eval' :: Expr -> Reader [(Id, Val)] Val
    eval' (Let x e e') = do
      v <- eval' e
      local ((x, v):) (eval' e')
    eval' (BinOp op lhs rhs) = do
      lhs' <- eval' lhs
      rhs' <- eval' rhs
      case (op, lhs', rhs') of
        (Add, VInt l, VInt r) -> return $ VInt (l + r)
        (Sub, VInt l, VInt r) -> return $ VInt (l - r)
        (Mul, VInt l, VInt r) -> return $ VInt (l * r)
        (Lt, VInt l, VInt r) -> return $ VBool (l < r)
        (Append, VStr l, VStr r) -> return $ VStr (l ++ r)
        _ -> error $ "Unsupported operation " ++ show (op, lhs', rhs')
    eval' (Var x) = do
      env <- ask
      case lookup x env of
        Just v -> return v
        Nothing -> error $ "Unbounded variable: " ++ x
    eval' (Str s) = return (VStr s)
    eval' (Int v) = return (VInt v)
    eval' (Bool b) = return (VBool b)
