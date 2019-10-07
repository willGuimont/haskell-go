{-# OPTIONS -Wall #-}
{-# LANGUAGE LambdaCase #-}

module BoardUtils
  ( placeStone
  , hasLiberties
  , getGroup
  , killGroup
  ) where

import Data.List
import Data.Monoid
import Data.Tuple.Curry
import Safe

import Board
import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Either (fromRight)
import Data.STRef (newSTRef, readSTRef, writeSTRef)

isPositionValid :: Board -> Position -> Bool
isPositionValid b p = x >= 0 && y >= 0 && x < sx && y < sy
  where
    (x, y) = p
    (sx, sy) = size b

isNeighbor :: Stone -> Stone -> Bool
isNeighbor x y = abs (fst p1 - fst p2) + abs (snd p1 - snd p2) == 1
  where
    p1 = position x
    p2 = position y

getNeighbors :: Board -> Stone -> [Stone]
getNeighbors b s = filter (isNeighbor s) ss
  where
    ss = stones b

isEmptyOrType :: StoneType -> Stone -> Bool
isEmptyOrType st s =
  let y = stoneType s
   in y == Empty || y == st

hasLiberties :: Board -> Position -> Bool
hasLiberties b p = hasLibertiesStone b (getStone b p)

hasLibertiesStone :: Board -> Stone -> Bool
hasLibertiesStone b s = foldBoard b p False combine predicate
  where
    p = position s
    st = stoneType s
    combine x y = x || stoneType y == Empty
    predicate = isEmptyOrType st

getGroup :: Board -> Position -> [Stone]
getGroup b p = foldBoard b p [] (flip (:)) (\s -> stoneType s == st)
  where
    st = stoneType $ getStone b p

foldBoard :: Board -> Position -> a -> (a -> Stone -> a) -> (Stone -> Bool) -> a
foldBoard b p start = foldStones ss start remainder
  where
    s = getStone b p
    ss = [s]
    remainder = stones b \\ ss

foldStones :: [Stone] -> a -> [Stone] -> (a -> Stone -> a) -> (Stone -> Bool) -> a
foldStones [] accumulator _ _ _ = accumulator
foldStones stack accumulator remainder combine predicate =
  foldStones newStack newAccumulator newRemainder combine predicate
  where
    current = head stack
    newNeighbors = filter predicate $ filter (isNeighbor current) remainder
    newStack = tail stack ++ newNeighbors
    newRemainder = remainder \\ newNeighbors
    newAccumulator = accumulator `combine` current

killGroup :: Board -> [Stone] -> Either Error Board
killGroup b = foldl (\n s -> n >>= (\x -> setStone x (position s) Empty)) (Right b)

updateBoard :: Board -> Position -> Either Error Board
updateBoard board p =
  foldl
    (\mb s ->
       mb >>=
       (\b ->
          if not $ hasLibertiesStone b s
            then (let ss = getGroup b (position s)
                   in killGroup b ss)
            else mb))
    (Right board)
    toCheck
  where
    current = getStone board p
    neighbors = getNeighbors board current
    toCheck = neighbors ++ [current]

countPoints :: [MarkedStone] -> Score
countPoints ms =
  runST $ do
    blackScoreRef <- newSTRef 0
    whiteScoreRef <- newSTRef 0
    forM_ ms $ \case
      WhiteMark -> updateScore whiteScoreRef
      BlackMark -> updateScore blackScoreRef
      _ -> pure ()
    b <- readSTRef blackScoreRef
    w <- readSTRef whiteScoreRef
    return $ Score b w
  where
    updateScore scoreRef = do
      s <- readSTRef scoreRef
      let newScore = s + 1
      writeSTRef scoreRef newScore

type CanPlayInterface = Board -> Board -> [Board] -> StoneType -> Position -> Bool

canPlay :: CanPlayInterface
canPlay v w x y z = getAll $ foldMap (All .) predicates (v, w, x, y, z)
  where
    predicates = map uncurryN [canPlayPosition, isPositionEmpty, isNotKOMove, isNotSuicide]

isPositionEmpty :: CanPlayInterface
isPositionEmpty b _ _ _ p = stoneType s == Empty
  where
    s = getStone b p

isNotKOMove :: CanPlayInterface
isNotKOMove _ n bs _ _ = Just n /= possibleKO
  where
    possibleKO = headMay bs

canPlayPosition :: CanPlayInterface
canPlayPosition b _ _ _ = isPositionValid b

isNotSuicide :: CanPlayInterface
isNotSuicide _ n _ _ p =
  let s = getStone n p
   in stoneType s /= Empty

placeStone :: Board -> [Board] -> StoneType -> Position -> Either String Board
placeStone b bs st p =
  if canPlayStone
    then updatedBoard
    else Left "could not play"
  where
    newBoard = setStone b p st
    updatedBoard = newBoard >>= (`updateBoard` p)
    canPlayStoneEither = fmap (\nb -> canPlay b nb bs st p) updatedBoard
    canPlayStone = fromRight False canPlayStoneEither