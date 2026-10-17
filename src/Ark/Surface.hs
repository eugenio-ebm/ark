module Ark.Surface (
  Expr(..),
  BinOp(..),
  prettyExpr,
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

prettyExpr :: Expr -> String
prettyExpr expr = p (getN expr) expr
  where
    getN :: Expr -> Int
    getN (BinOp _ _ _) = 1
    getN (Int _) = 2

    -- print e such that it gets parsed as an Expr<n> in the grammar
    p :: Int -> Expr -> String
    p n e
      | n' <- getN e
      , n > n' = "(" ++ p n' e ++ ")"
    p n (BinOp Add lhs rhs) =
      p (n + 1) lhs ++ " + " ++  p (n + 1) rhs
    p n (BinOp Sub lhs rhs) =
      p (n + 1) lhs ++ " - " ++  p (n + 1) rhs
    p _ (Int v)
      | v >= 0 = show v
      | otherwise = "(" ++ show v ++ ")"

eval :: Expr -> Int
eval expr = runIdentity (eval' expr)
  where
    eval' :: Expr -> Identity Int
    eval' (BinOp Add lhs rhs) = (+) <$> eval' lhs <*> eval' rhs
    eval' (BinOp Sub lhs rhs) = (-) <$> eval' lhs <*> eval' rhs
    eval' (Int v) = pure v
