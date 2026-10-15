module Main (main) where

import Ark.Surface.TestExpr ( testExpr )
import Ark.Surface.TestEval ( testEval )

main :: IO ()
main = do
  testExpr
  testEval
