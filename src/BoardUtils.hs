{-# OPTIONS -Wall #-}

module BoardUtils
  ( placeStone
  , hasLiberty
  ) where

import Safe
import Data.List

import Board

isPositionValid :: Board -> Position -> Bool
isPositionValid b p = x >= 0 && y >= 0 && x < sx && y < sy
  where
    (x, y) = p
    (sx, sy) = size b

getNeighborPositions :: Board -> Position -> [Position]
getNeighborPositions b p = filter (isPositionValid b) [(x + i, y + j) | (i, j) <- [(1, 0), (-1, 0), (0, 1), (0, -1)]]
  where
    (x, y) = p

hasLiberty :: Board -> Position -> Bool
hasLiberty b p = snd $ hasLiberty' st s [] False getNextStones
  where
    s = getStone b p
    st = stoneType s
    getNextStones x = map (getStone b) (getNeighborPositions b (position x))

hasLiberty' :: StoneType -> Stone -> [Stone] -> Bool -> (Stone -> [Stone]) -> ([Stone], Bool)
hasLiberty' _ s visited True _ = (s : visited, True)
hasLiberty' st s visited found gs =
  if not (null empty)
    then (visited, True)
    else foldl (\(v, b) c -> hasLiberty' st c (c:v) b gs) (visited, False) sameColor
  where
    nextStones = gs s \\ visited
    is y x = stoneType x == y
    empty = filter (is Empty) nextStones
    sameColor = filter (is st) nextStones

type CanPlayInterface = Board -> [Board] -> StoneType -> Position -> Bool

canPlay :: CanPlayInterface
canPlay b bs st p = all (\f -> f b bs st p) [canPlayPosition, isPositionEmpty, isNotKOMove, isNotSuicide]

isPositionEmpty :: CanPlayInterface
isPositionEmpty b _ _ p = stoneType s == Empty
  where
    s = getStone b p

isNotKOMove :: CanPlayInterface
isNotKOMove b bs s p = nextBoard /= possibleKO
  where
    nextBoard = setStone b p s
    possibleKO = lastMay bs

canPlayPosition :: CanPlayInterface
canPlayPosition b _ _ = isPositionValid b

isNotSuicide :: CanPlayInterface
isNotSuicide b _ _ = hasLiberty b

placeStone :: Board -> [Board] -> StoneType -> Position -> Maybe Board
placeStone b bs st p =
  if canPlayStone
    then newBoard
    else Nothing
  where
    canPlayStone = canPlay b bs st p
    newBoard = setStone b p st