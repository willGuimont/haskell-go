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
      let Right b = setStone b0 (0, 0) White
      hasLiberties b (0, 0) `shouldBe` True
    it "should work example 2" $ do
      let b0 = makeBoard (4, 4)
      let Right b1 = setStone b0 (0, 0) White
      let Right b2 = setStone b1 (1, 0) White
      let Right b3 = setStone b2 (0, 1) White
      let Right b4 = setStone b3 (1, 1) White
      hasLiberties b4 (0, 0) `shouldBe` True
    it "should work example 3" $ do
      let b0 = makeBoard (4, 4)
      let Right b1 = setStone b0 (0, 0) White
      let Right b2 = setStone b1 (1, 0) White
      let Right b3 = setStone b2 (0, 1) White
      let Right b4 = setStone b3 (1, 1) White
      let Right b5 = setStone b4 (2, 0) White
      let Right b6 = setStone b5 (2, 1) White
      let Right b7 = setStone b6 (2, 2) White
      let Right b8 = setStone b7 (1, 2) White
      let Right b9 = setStone b8 (0, 2) White
      hasLiberties b9 (0, 0) `shouldBe` True
    it "should work example 4" $ do
      let b0 = makeBoard (4, 4)
      let Right b1 = setStone b0 (0, 0) White
      let Right b2 = setStone b1 (1, 0) White
      let Right b3 = setStone b2 (0, 1) Black
      let Right b4 = setStone b3 (1, 1) Black
      let Right b5 = setStone b4 (2, 0) Black
      hasLiberties b5 (0, 0) `shouldBe` False
    it "should work example 5" $ do
      let b0 = makeBoard (2, 2)
      let Right b1 = setStone b0 (0, 0) White
      let Right b2 = setStone b1 (1, 0) White
      let Right b3 = setStone b2 (0, 1) White
      let Right b4 = setStone b3 (1, 1) White
      hasLiberties b4 (0, 0) `shouldBe` False
    it "should work example 6" $ do
      let b0 = makeBoard (3, 3)
      let Right b1 = setStone b0 (0, 1) White
      let Right b2 = setStone b1 (1, 0) White
      let Right b3 = setStone b2 (2, 1) White
      let Right b4 = setStone b3 (1, 2) White
      let Right b5 = setStone b4 (1, 1) Black
      hasLiberties b5 (1, 1) `shouldBe` False
    describe "BoardUtils.hasLiberty" $ do
      it "should work example 1" $ do
        let b0 = makeBoard (4, 4)
        let Right b = setStone b0 (0, 0) White
        length (getGroup b (0, 0)) `shouldBe` 1
      it "should work example 2" $ do
        let b0 = makeBoard (4, 4)
        let Right b1 = setStone b0 (0, 0) White
        let Right b2 = setStone b1 (1, 0) White
        let Right b3 = setStone b2 (0, 1) White
        let Right b4 = setStone b3 (1, 1) White
        length (getGroup b4 (0, 0)) `shouldBe` 4
    describe "BoardUtils.killGroup" $
      it "should work example 1" $ do
        let b0 = makeBoard (4, 4)
        let Right b1 = setStone b0 (0, 0) White
        let Right b2 = setStone b1 (1, 0) White
        let Right b3 = setStone b2 (0, 1) White
        let Right b4 = setStone b3 (1, 1) White
        let Right b5 = killGroup b4 $ getGroup b4 (0, 0)
        all (\s -> stoneType s == Empty) (stones b5) `shouldBe` True