module Ark.Surface (
  Expr(..),
  BinOp(..),
  eval,
  ) where

import Control.Monad.Reader ( Reader, runReader, ask, local )

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
