module Ark.Assert ( assertEqual ) where

import Test.QuickCheck.Monadic

assertEqual :: (Eq a, Show a) => a -> a -> String -> PropertyM IO ()
assertEqual actual expected test = assertWith (actual == expected) msg
  where
    msg = test ++ "\n"
          ++ "Expected: " ++ show expected ++ "\n"
          ++ "Actual: " ++ show actual
