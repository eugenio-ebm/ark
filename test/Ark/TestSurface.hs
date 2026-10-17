module Ark.TestSurface where

import Ark.Surface

import Test.QuickCheck
import Test.QuickCheck.Monadic

instance Arbitrary Expr where
  arbitrary = sized genExpr
    where
      genExpr :: Int -> Gen Expr
      genExpr 0 = genInt
      genExpr n = oneof [genBinOp n, genInt]

      genBinOp n = BinOp <$> op <*> lhs <*> rhs
        where
          op = elements [Add, Sub]
          lhs = subExpr
          rhs = subExpr
          subExpr = genExpr (n `div` 2)

      genInt = Int <$> arbitrary

  shrink (Int v) = Int <$> shrink v
  shrink (BinOp op lhs rhs) = [lhs, rhs]
                              ++ (BinOp op <$> shrink lhs <*> pure rhs)
                              ++ (BinOp op lhs <$> shrink rhs)

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

testSurface :: IO ()
testSurface = do
  testEval
