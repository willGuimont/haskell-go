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
  , Error
  , makeBoard
  , getStone
  , setStone
  ) where

import Data.List
import Data.List.Tools
import Data.Either.Utils (maybeToEither)

data StoneType
  = Black
  | White
  | Empty
  deriving (Eq, Show)

data Stone =
  Stone
    { stoneType :: StoneType
    , position :: Position
    }
  deriving (Eq, Show)

data Board =
  Board
    { stones :: [Stone]
    , size :: Size
    }
  deriving (Eq, Show)

type Position = (Int, Int)

type Size = (Int, Int)

type Error = String

makeBoard :: Size -> Board
makeBoard s = Board emptyStones s
  where
    (sx, sy) = s
    positions = [(x, y) | x <- [0 .. (sx - 1)], y <- [0 .. sy - 1]]
    emptyStones = map (Stone Empty) positions

getStone :: Board -> Position -> Stone
getStone board pos = head $ filter (\x -> position x == pos) (stones board)

setStone :: Board -> Position -> StoneType -> Either Error Board
setStone b p s = maybeToEither "cannot set stone" newBoard
  where
    ss = stones b
    index = getStone b p `elemIndex` ss
    newStones = (\i -> setAt ss i $ Stone s p) <$> index
    newBoard = (\x -> Board x (size b)) <$> newStones