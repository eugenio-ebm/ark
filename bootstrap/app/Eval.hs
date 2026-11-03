{-# LANGUAGE RecursiveDo #-}
module Eval ( eval ) where

import Syntax

import Control.Monad.Reader ( ReaderT, runReaderT, ask, local, liftIO )
-- import Control.Monad.State ( State, evalState, get, modify )

eval :: Expr -> IO Val
eval expr = runReaderT (eval' expr) prelude
  where
    eval' :: Expr -> ReaderT [(Id, Val)] IO Val
    eval' (Let x e e') = mdo
      v <- local (\env -> (x, v) : env) (eval' e)
      local ((x, v):) (eval' e')
    eval' (Lam param body) = do
      env <- ask
      return (VClo param body env)
    eval' (Match e' cs') = do
      v <- eval' e'
      go v cs'
      where
        go _ [] = return VUnit
        go v ((p,e):cs) =
          case (v, p) of
            (VCons f args, PCons f' params)
              | f == f'
              , length args == length params -> do
                  let env' = zip params args
                  local (env' ++) (eval' e)
            (VChar c, PChar c') | c == c' -> eval' e
            (_, POtherwise) -> eval' e
            _ -> go v cs
    eval' (BinOp op lhs rhs) = do
      lhs' <- eval' lhs
      rhs' <- eval' rhs
      case (op, lhs', rhs') of
        (Add, VInt l, VInt r) -> return $ VInt (l + r)
        (Sub, VInt l, VInt r) -> return $ VInt (l - r)
        (Mul, VInt l, VInt r) -> return $ VInt (l * r)
        (Lt, VInt l, VInt r) -> return $ VCons (if (l < r) then "True" else "False") []
        -- (Append, VStr l, VStr r) -> return $ VStr (l ++ r)
        _ -> error $ "Unsupported operation " ++ show (op, lhs', rhs')
    eval' (Cons f args) = do
      vs <- mapM eval' args
      return (VCons f vs)
    eval' (App e1 e2) = do
      arg <- eval' e2
      f <- eval' e1
      case f of
        VClo param body env ->
          local (\_ -> (param, arg):env) (eval' body)
        VPrim "read" -> do
          s <- liftIO $ getLine
          return $ foldr (\c s' -> VCons "Cons" [VChar c, s']) (VCons "Nil" []) s
        VPrim "print" -> do
          liftIO $ putStrLn (show arg)
          return arg
        _ -> error $ "Not a function:" ++ show f
    eval' (Var "read") = return (VPrim "read")
    eval' (Var "print") = return (VPrim "print")
    eval' (Var x) = do
      env <- ask
      case lookup x env of
        Just v -> return v
        Nothing -> error $ "Unbounded variable: " ++ x
    eval' (Char c) = return (VChar c)
    eval' (Str s) = return $ foldr (\c s' -> VCons "Cons" [VChar c, s']) (VCons "Nil" []) s
    eval' (Int v) = return (VInt v)
    eval' Unit = return VUnit

    prelude = [
      ("True", VCons "True" []),
      ("False", VCons "False" []),
      ("Some", (VClo "x" (Cons "Some" [Var "x"]) [])),
      ("None", VCons "None" []),
      ("Nil", VCons "Nil" []),
      ("Cons", (VClo "x" (Lam "xs" (Cons "Cons" [(Var "x"), (Var "xs")])) []))]
