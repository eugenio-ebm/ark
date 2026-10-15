module Ark.Surface.TestExpr ( testExpr ) where

import Ark.Surface.Expr

import Ark.Surface.Parser ( parse )
import Ark.Surface.Eval ( eval )

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

prop_prettyExpr_parseAndEval :: Expr -> Property
prop_prettyExpr_parseAndEval expr = monadicIO $ do
  let expected = eval expr
      actual = eval . parse $ prettyExpr expr

  assertWith (actual == expected) "prop_prettyExpr_parseAndEval"

testExpr :: IO ()
testExpr = do
  quickCheck prop_prettyExpr_parseAndEval
