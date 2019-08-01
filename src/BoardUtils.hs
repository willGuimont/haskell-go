{-# OPTIONS -Wall #-}

module BoardUtils
  (
  ) where

import           Data.List
import           Data.List.Tools
import           Safe

import Board

isPositionValid :: Board -> Position -> Bool
isPositionValid b p = x >= 0 && y >= 0 && x < sx && y < sy
  where
    (x, y) = p
    (sx, sy) = size b

getNeighborPositions :: Board -> Position -> [Position]
getNeighborPositions b p = filter (isPositionValid b) [(x + i, y + j) | (i, j) <-[(1, 0), (-1, 0), (0, 1), (0, -1)]]
  where (x, y) = p

hasLiberty :: Board -> Position -> Bool
hasLiberty b p = hasLiberty' b (stoneType s) p []
  where s = getStone b p

hasLiberty' :: Board -> StoneType -> Position -> [Position] -> Bool
hasLiberty' b st p visited = isPositionValid b p && (isEmpty || fst (foldl (\(bool, v) pp -> (bool || hasLiberty b st p visited, pp : v)) (False, visited) neighbors))
  where currentStoneType = stoneType $ getStone b p
        isEmpty = currentStoneType == Empty
        neighbors = filter (\x -> stoneType (getStone b x) /= st) $ getNeighborPositions b p \\ visited


type CanPlayInterface = Board -> [Board] -> StoneType -> Position -> Bool

canPlay :: CanPlayInterface
canPlay b bs st p = all (\f -> f b bs st p) [canPlayPosition, isPositionEmpty, isNotKOMove, isNotSuicideMove]

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

-- Board -> StoneType -> Position -> [Position] -> Bool
isNotSuicideMove :: CanPlayInterface
isNotSuicideMove b _ _ = hasLiberty b

placeStone :: Board -> [Board] -> StoneType -> Position -> Maybe Board
placeStone b bs st p =
  if canPlayStone
    then newBoard
    else Nothing
  where
    canPlayStone = canPlay b bs st p
    newBoard = setStone b p st
