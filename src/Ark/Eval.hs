module Ark.Eval where

import Ark.Expr ( Expr(..) )

import Control.Monad.Identity

type Eval = Identity

eval :: Expr -> Int
eval expr = runIdentity (eval' expr)
  where
    eval' (Add lhs rhs) = (+) <$> eval' lhs <*> eval' rhs
    eval' (Sub lhs rhs) = (-) <$> eval' lhs <*> eval' rhs
    eval' (Int v) = pure v
