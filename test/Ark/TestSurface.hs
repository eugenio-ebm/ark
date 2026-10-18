module Ark.TestSurface where

import Ark.Surface

import Ark.Assert ( assertEqual )

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
  shrink (Var _) = []
  shrink (Int v) = Int <$> shrink v

prop_normalize :: Expr -> Property
prop_normalize expr = monadicIO $ do
  let expected = eval expr
      actual = eval (normalize expr)

  assertEqual actual expected "prop_normalize"

prop_eval_int :: Int -> Property
prop_eval_int x = monadicIO $ do
  let expr = Int x

  let expected = x
      actual = eval expr

  assertEqual actual expected "prop_eval_int"

prop_eval_add :: Expr -> Expr -> Property
prop_eval_add lhs rhs = monadicIO $ do
  let expr = BinOp Add lhs rhs

  let expected = eval lhs + eval rhs
      actual = eval expr

  assertEqual actual expected "prop_eval_add"

prop_eval_sub :: Expr -> Expr -> Property
prop_eval_sub lhs rhs = monadicIO $ do
  let expr = BinOp Sub lhs rhs

  let expected = eval lhs - eval rhs
      actual = eval expr

  assertEqual actual expected "prop_eval_sub"

testSurface :: IO ()
testSurface = do
  quickCheck prop_normalize

  quickCheck prop_eval_int
  quickCheck prop_eval_add
  quickCheck prop_eval_sub
