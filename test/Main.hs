module Main (main) where

import Ark.Surface.TestPrettyExpr ( testPrettyExpr )
import Ark.Surface.TestEval ( testEval )

main :: IO ()
main = do
  testPrettyExpr
  testEval
