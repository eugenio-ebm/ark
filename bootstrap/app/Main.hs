module Main where

import Parser ( parse )
import Eval ( eval )

evalFile :: String -> IO ()
evalFile filename = do
  s <- readFile filename
  let expr = parse s
  let v = eval expr
  putStrLn (show v)


main :: IO ()
main = putStrLn "Hello, Haskell!"
