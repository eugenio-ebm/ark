module Main where

import Parser ( parse )
import Eval ( eval )

evalStr :: String -> IO ()
evalStr s = do
  let expr = parse s
  v <- eval expr
  putStrLn (show v)

parseFile :: String -> IO ()
parseFile filename = do
  s <- readFile filename
  let expr = parse s
  putStrLn (show expr)

evalFile :: String -> IO ()
evalFile filename = do
  s <- readFile filename
  evalStr s

main :: IO ()
main = putStrLn "Hello, Haskell!"
