module ArbitraryBoard where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Board

instance Arbitrary StoneType where
  arbitrary = elements [White, Black, Empty]

instance Arbitrary Stone where
  arbitrary = do
    s <- arbitrary
    Stone s <$> arbitrary

instance Arbitrary Board where
  arbitrary = do
    ss <- arbitrary
    Board ss <$> arbitrary
