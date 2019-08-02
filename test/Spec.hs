import Control.Exception (evaluate)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Board
import Data.Maybe (fromMaybe)

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

makeBoardCorrectSizeProperty :: (Int, Int) -> Bool
makeBoardCorrectSizeProperty s = size board == s
  where
    board = makeBoard s

makeBoardEmptyProperty :: (Int, Int) -> Bool
makeBoardEmptyProperty s = all (\s -> stoneType s == Empty) $ stones board
  where
    board = makeBoard s

getStonePositionProperty :: (Board, Position) -> Bool
getStonePositionProperty (b, p) =
  fst p < 0 || snd p < 0 || all (\x -> position x /= p) ss || (length ss <= 0) || (position (getStone b p) == p)
  where
    ss = stones b

setStonePositionProperty :: (Board, Position, StoneType) -> Bool
setStonePositionProperty (b, p, st) =
  fst p < 0 || snd p < 0 || all (\x -> position x /= p) ss || (length ss <= 0) || stoneType s == st
  where
    ss = stones b
    Just s = (`getStone` p) <$> setStone b p st

main :: IO ()
main =
  hspec $ do
    describe "Board.makeBoard" $ do
      it "make correct size" $ property makeBoardCorrectSizeProperty
      it "make all empty" $ property makeBoardEmptyProperty
    describe "Board.getStone" $ it "returns the correct stone" $ property getStonePositionProperty
    describe "Board.setStone" $ it "sets the stone" $ property setStonePositionProperty