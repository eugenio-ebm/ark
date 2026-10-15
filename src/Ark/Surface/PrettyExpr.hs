module Ark.Surface.PrettyExpr ( prettyExpr ) where

import Ark.Surface.Expr ( Expr(..), BinOp(..) )

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
