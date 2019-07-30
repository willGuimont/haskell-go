{-# OPTIONS -Wall #-}

module Board
  ( Stone(Stone)
  , stoneType
  , position
  , StoneType(Empty, Black, White)
  , Board(Board)
  , size
  , stones
  , Position
  , Size
  , makeBoard
  , getStone
  , placeStone
  ) where

import           Data.List
import           Data.List.Tools
import           Safe

data StoneType
  = Black
  | White
  | Empty
  deriving (Eq)

data Stone =
  Stone
    { stoneType :: StoneType
    , position  :: Position
    }
  deriving (Eq)

data Board =
  Board
    { stones :: [Stone]
    , size   :: Size
    }

type Position = (Int, Int)

type Size = (Int, Int)

makeBoard :: Size -> Board
makeBoard s = Board emptyStones s
  where
    (sx, sy) = s
    positions = [(x, y) | x <- [0 .. (sx - 1)], y <- [0 .. sy - 1]]
    emptyStones = map (Stone Empty) positions

getStone :: Board -> Position -> Maybe Stone
getStone board pos = headMay $ filter (\x -> position x == pos) (stones board)

isPositionEmpty :: Board -> StoneType -> Position -> Bool
isPositionEmpty b _ p =
  case atPos of
    Nothing -> False
    Just x  -> stoneType x == Empty
  where
    atPos = getStone b p

canPlay :: Board -> StoneType -> Position -> Bool
canPlay b s p = all (\f -> f b s p) [isPositionEmpty]

placeStone :: Board -> StoneType -> Position -> Maybe Board
placeStone b s p =
  if canPlayStone
    then newBoard
    else Nothing
  where
    canPlayStone = canPlay b s p
    ss = stones b
    index = getStone b p >>= (`elemIndex` ss)
    newStones = (\i -> setAt ss i $ Stone s p) <$> index
    newBoard = (\x -> Board x (size b)) <$> newStones
