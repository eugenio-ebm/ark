module Eval ( eval ) where

import Syntax

import Control.Monad.Reader ( ReaderT, runReaderT, ask, local, liftIO )
-- import Control.Monad.State ( State, evalState, get, modify )

eval :: Expr -> IO Val
eval expr = runReaderT (eval' expr) []
  where
    eval' :: Expr -> ReaderT [(Id, Val)] IO Val
    eval' (Let x e e') = do
      v <- eval' e
      local ((x, v):) (eval' e')
    eval' (Lam param body) = do
      env <- ask
      return (VClo param body env)
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
    eval' (App e1 e2) = do
      arg <- eval' e2
      f <- eval' e1
      case f of
        VClo param body env ->
          local (\_ -> (param, arg):env) (eval' body)
        VPrim "print" -> do
          liftIO $ putStrLn (show arg)
          return arg
        _ -> error $ "Not a function:" ++ show f
    eval' (Var "print") = return (VPrim "print")
    eval' (Var x) = do
      env <- ask
      case lookup x env of
        Just v -> return v
        Nothing -> error $ "Unbounded variable: " ++ x
    eval' (Str s) = return (VStr s)
    eval' (Int v) = return (VInt v)
    eval' (Bool b) = return (VBool b)


