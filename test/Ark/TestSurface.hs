module Ark.TestSurface where

import Ark.Surface

import Test.QuickCheck
import Test.QuickCheck.Monadic

instance Arbitrary Expr where
  arbitrary = sized (genExpr [])
    where
      genExpr :: [String] -> Int -> Gen Expr
      genExpr _ 0 = genInt
      genExpr xs n = oneof [genLet xs n, genBinOp xs n, genVar xs, genInt]

      genLet xs n = do
        x <- elements ["x", "y"]
        e <- genExpr xs (n `div` 2)
        e' <- genExpr (x:xs) (n `div` 2)
        return (Let x e e')

      genBinOp xs n = BinOp <$> op <*> lhs <*> rhs
        where
          op = elements [Add, Sub]
          lhs = subExpr
          rhs = subExpr
          subExpr = genExpr xs (n `div` 2)

      genVar [] = genInt
      genVar xs = Var <$> elements xs

      genInt = Int <$> arbitrary

  shrink (Let x e e') = [e, e']
                        ++ (Let x <$> shrink e <*> pure e')
                        ++ (Let x e <$> shrink e')
                        ++ (Let x <$> shrink e <*> shrink e')
  shrink (BinOp op lhs rhs) = [lhs, rhs]
                              ++ (BinOp op <$> shrink lhs <*> pure rhs)
                              ++ (BinOp op lhs <$> shrink rhs)
                              ++ (BinOp op <$> shrink lhs <*> shrink rhs)
  shrink (Var x) = []
  shrink (Int v) = Int <$> shrink v

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
