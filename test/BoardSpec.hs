module BoardSpec
  ( boardSpec
  ) where

import Test.Hspec
import Test.QuickCheck

import ArbitraryBoard
import Board

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
    Right s = (`getStone` p) <$> setStone b p st

boardSpec = do
  describe "Board.makeBoard" $ do
    it "make correct size" $ property makeBoardCorrectSizeProperty
    it "make all empty" $ property makeBoardEmptyProperty
  describe "Board.getStone" $ it "returns the correct stone" $ property getStonePositionProperty
  describe "Board.setStone" $ it "sets the stone" $ property setStonePositionProperty