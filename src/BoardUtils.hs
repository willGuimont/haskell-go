{-# OPTIONS -Wall #-}

module BoardUtils
  ( placeStone
  , hasLiberties
  , getGroup
  ) where

import Data.List
import Data.Maybe
import Debug.Trace
import Safe

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

isNeighbor :: Stone -> Stone -> Bool
isNeighbor x y = abs (fst p1 - fst p2) + abs (snd p1 - snd p2) == 1
  where
    p1 = position x
    p2 = position y

getNeighbors :: Board -> Stone -> [Stone]
getNeighbors b s = map (getStone b) $ getNeighborPositions b $ position s

isEmptyOrType :: StoneType -> Stone -> Bool
isEmptyOrType st s =
  let y = stoneType s
   in y == Empty || y == st

hasLiberties :: Board -> Position -> Bool
hasLiberties b p = foldBoard b p False combine predicate
  where
    st = stoneType $ getStone b p
    combine x y = x || stoneType y == Empty
    predicate = isEmptyOrType st

getGroup :: Board -> Position -> [Stone]
getGroup b p = foldBoard b p [] (flip (:)) (\s -> stoneType s == st)
  where
    st = stoneType $ getStone b p

foldBoard :: Board -> Position -> a -> (a -> Stone -> a) -> (Stone -> Bool) -> a
foldBoard b p start = foldBoard' ss start remainder
  where
    s = getStone b p
    ss = [s]
    remainder = stones b \\ ss

foldBoard' :: [Stone] -> a -> [Stone] -> (a -> Stone -> a) -> (Stone -> Bool) -> a
foldBoard' [] accumulator _ _ _ = accumulator
foldBoard' stack accumulator remainder combine predicate =
  foldBoard' newStack newAccumulator newRemainder combine predicate
  where
    current = head stack
    newNeighbors = filter predicate $ filter (isNeighbor current) remainder
    newStack = tail stack ++ newNeighbors
    newRemainder = remainder \\ newNeighbors
    newAccumulator = accumulator `combine` current

type CanPlayInterface = Board -> Maybe Board -> [Board] -> StoneType -> Position -> Bool

canPlay :: CanPlayInterface
canPlay b n bs st p = all (\f -> f b n bs st p) [canPlayPosition, isPositionEmpty, isNotKOMove, isNotSuicide]

isPositionEmpty :: CanPlayInterface
isPositionEmpty b _ _ _ p = stoneType s == Empty
  where
    s = getStone b p

isNotKOMove :: CanPlayInterface
isNotKOMove _ n bs s p = n /= possibleKO
  where
    possibleKO = lastMay bs

canPlayPosition :: CanPlayInterface
canPlayPosition b _ _ _ = isPositionValid b

isNotSuicide :: CanPlayInterface
isNotSuicide _ n _ _ p = maybe False (`hasLiberties` p) n

placeStone :: Board -> [Board] -> StoneType -> Position -> Maybe Board
placeStone b bs st p =
  if canPlayStone
    then newBoard
    else Nothing
  where
    newBoard = setStone b p st
    canPlayStone = canPlay b newBoard bs st p