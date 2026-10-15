module Ark.Surface.TestEval ( testEval ) where

import Ark.Surface.Eval ( eval )

import Ark.Surface.Expr ( Expr(..), BinOp(..) )

import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_eval_justInt :: Int -> Property
prop_eval_justInt x = monadicIO $ do
  let expr = Int x

  let expected = x
      actual = eval expr

  assertWith (actual == expected) "prop_eval_justInt"

prop_eval_addTwoInts :: Int -> Int -> Property
prop_eval_addTwoInts x y = monadicIO $ do
  let expr = BinOp Add (Int x) (Int y)

  let expected = x + y
      actual = eval expr

  assertWith (actual == expected) "prop_eval_addTwoInts"

prop_eval_subTwoInts :: Int -> Int -> Property
prop_eval_subTwoInts x y = monadicIO $ do
  let expr = BinOp Sub (Int x) (Int y)

  let expected = x - y
      actual = eval expr

  assertWith (actual == expected) "prop_eval_subTwoInts"

testEval :: IO ()
testEval = do
  quickCheck prop_eval_justInt
  quickCheck prop_eval_addTwoInts
  quickCheck prop_eval_subTwoInts
