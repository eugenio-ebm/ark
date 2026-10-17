module Main where

import Ark.Parser ( parse )
import Ark.Surface ( eval )

import Control.Monad ( forever, when )
import System.Exit
import System.IO

main :: IO ()
main = forever $ do
  hSetBuffering stdout NoBuffering

  putStr "ark> "
  s <- getLine
  when (s == "") exitSuccess
  let e = parse s
  let v = eval e
  putStrLn $ show v
