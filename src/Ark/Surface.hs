module Ark.Surface (
  Expr(..),
  BinOp(..),
  eval,
  ) where

import Control.Monad.Identity ( Identity, runIdentity )

data Expr
  = BinOp BinOp Expr Expr
  | Int Int
  deriving (Eq, Show)

data BinOp
  = Add
  | Sub
  deriving (Eq, Show)

eval :: Expr -> Int
eval expr = runIdentity (eval' expr)
  where
    eval' :: Expr -> Identity Int
    eval' (BinOp Add lhs rhs) = (+) <$> eval' lhs <*> eval' rhs
    eval' (BinOp Sub lhs rhs) = (-) <$> eval' lhs <*> eval' rhs
    eval' (Int v) = pure v
