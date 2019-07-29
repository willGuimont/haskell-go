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

placeStone :: Board -> StoneType -> Position -> Maybe Board
placeStone board s pos = b
  where
    ss = stones board
    index = getStone board pos >>= (`elemIndex` ss)
    newStones = (\i -> setAt ss i $ Stone s pos) <$> index
    b = (\x -> Board x (size board)) <$> newStones
