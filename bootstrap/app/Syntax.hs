module Syntax (
  Expr(..),
  BinOp(..),
  normalize,
  eval,
  ) where

import Control.Monad.Reader ( Reader, runReader, ask, local )
import Control.Monad.State ( State, evalState, get, modify )

data Expr
  = Let String Expr Expr
  | BinOp BinOp Expr Expr
  | Var String
  | Int Int
  deriving (Eq, Show)

data BinOp
  = Add
  | Sub
  deriving (Eq, Show)

sub :: String -> String -> Expr -> Expr
sub old new = f
  where
    f :: Expr -> Expr
    f (Let x e e')
      | x == old = Let x (f e) e'
      | otherwise = Let x (f e) (f e')
    f (BinOp op lhs rhs) = BinOp op (f lhs) (f rhs)
    f (Var x)
      | x == old = Var new
      | otherwise = Var x
    f (Int v) = Int v

normalize :: Expr -> Expr
normalize expr = k expr'
  where
    m = f expr
    (expr', k) = evalState m 0

    fresh :: String -> State Int String
    fresh name = do
      i <- get
      modify (+1)
      return (name ++ "$" ++ show i)

    f :: Expr -> State Int (Expr, Expr -> Expr)
    f (Let x e1 e2) = do
      (e1', k1) <- f e1
      e1'' <- fresh x
      let e2' = sub x e1'' e2
      (e2'', k2) <- f e2'
      return (e2'', (\e -> k1 (Let e1'' e1' (k2 e))))
    f (BinOp op lhs rhs) = do
      (lhs', kLhs) <- f lhs
      (rhs', kRhs) <- f rhs
      return (BinOp op lhs' rhs', kRhs . kLhs)
    f (Var x) = return (Var x, id)
    f (Int v) = return (Int v, id)

eval :: Expr -> Int
eval expr = runReader (eval' expr) []
  where
    eval' :: Expr -> Reader [(String, Int)] Int
    eval' (Let x e e') = do
      v <- eval' e
      local ((x, v):) (eval' e')
    eval' (BinOp Add lhs rhs) = (+) <$> eval' lhs <*> eval' rhs
    eval' (BinOp Sub lhs rhs) = (-) <$> eval' lhs <*> eval' rhs
    eval' (Var x) = do
      env <- ask
      case lookup x env of
        Just v -> return v
        Nothing -> error ("Unbounded variable: " ++ x)
    eval' (Int v) = pure v
