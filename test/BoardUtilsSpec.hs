module BoardUtilsSpec
  ( boardUtilsSpec
  ) where

import Test.Hspec
import Test.QuickCheck

import Data.Maybe

import ArbitraryBoard
import Board
import BoardUtils

boardUtilsSpec =
  describe "BoardUtils.hasLiberty" $ do
    it "should work example 1" $ do
      let b0 = makeBoard (4, 4)
      let Just b = setStone b0 (0, 0) White
      hasLiberty b (0, 0) `shouldBe` True
    it "should work example 2" $ do
      let b0 = makeBoard (4, 4)
      let Just b1 = setStone b0 (0, 0) White
      let Just b2 = setStone b1 (1, 0) White
      let Just b3 = setStone b2 (0, 1) White
      let Just b4 = setStone b3 (1, 1) White
      hasLiberty b4 (0, 0) `shouldBe` True
    it "should work example 3" $ do
      let b0 = makeBoard (4, 4)
      let Just b1 = setStone b0 (0, 0) White
      let Just b2 = setStone b1 (1, 0) White
      let Just b3 = setStone b2 (0, 1) White
      let Just b4 = setStone b3 (1, 1) White
      let Just b5 = setStone b4 (2, 0) White
      let Just b6 = setStone b5 (2, 1) White
      let Just b7 = setStone b6 (2, 2) White
      let Just b8 = setStone b7 (1, 2) White
      let Just b9 = setStone b8 (0, 2) White
      hasLiberty b9 (0, 0) `shouldBe` True
    it "should work example 4" $ do
      let b0 = makeBoard (4, 4)
      let Just b1 = setStone b0 (0, 0) White
      let Just b2 = setStone b1 (1, 0) White
      let Just b3 = setStone b2 (0, 1) Black
      let Just b4 = setStone b3 (1, 1) Black
      let Just b5 = setStone b4 (2, 0) Black
      hasLiberty b5 (0, 0) `shouldBe` False