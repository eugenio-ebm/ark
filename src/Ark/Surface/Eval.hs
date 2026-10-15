module Ark.Surface.Eval ( eval ) where

import Ark.Surface.Expr ( Expr(..), BinOp(..) )

import Control.Monad.Identity ( Identity, runIdentity )

type Eval = Identity

eval :: Expr -> Int
eval expr = runIdentity (eval' expr)
  where
    eval' :: Expr -> Eval Int
    eval' (BinOp Add lhs rhs) = (+) <$> eval' lhs <*> eval' rhs
    eval' (BinOp Sub lhs rhs) = (-) <$> eval' lhs <*> eval' rhs
    eval' (Int v) = pure v
