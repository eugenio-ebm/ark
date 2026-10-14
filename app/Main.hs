module Main where

import Ark.Surface.Parser ( parse )
import Ark.Surface.Eval ( eval )

import Control.Monad ( forever, when )
import System.Exit

main :: IO ()
main = forever $ do
  putStr "ark> "
  s <- getLine
  when (s == "") exitSuccess
  let e = parse s
  let v = eval e
  putStrLn $ show v
