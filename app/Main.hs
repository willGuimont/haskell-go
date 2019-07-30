{-# OPTIONS -Wall #-}

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game

import           Data.Maybe
import           Data.Tuple

import           Board

type World = (Board, StoneType)

fps :: Int
fps = 20

gameSize :: Int
gameSize = 9

halfBoardSize :: Int
halfBoardSize = 9 `div` 2

windowSize :: Int
windowSize = 500

padding :: Int
padding = 20

boardSizePixel :: Float
boardSizePixel = fromIntegral $ windowSize - 2 * padding :: Float

pixelPerCell :: Float
pixelPerCell = boardSizePixel / fromIntegral gameSize

stoneRadius :: Float
stoneRadius = pixelPerCell / 2 * 0.8

windowDisplay :: Display
windowDisplay = InWindow "Haskell Go" (windowSize, windowSize) (10, 10)

boardSize :: (Int, Int)
boardSize = (gameSize, gameSize)

positionToPixel :: Int -> Float
positionToPixel x = fromIntegral (x - halfBoardSize) * pixelPerCell

pixelToPosition :: Float -> Int
pixelToPosition p = halfBoardSize + floor (p / pixelPerCell + 1 / 2)

drawStone :: Stone -> [Picture]
drawStone Stone {stoneType = Empty} = [blank]
drawStone Stone {stoneType = s, position = (x, y)} =
  [translated $ color c $ circleSolid stoneRadius, translated $ color black $ circle stoneRadius]
  where
    c =
      case s of
        White -> white
        Black -> black
        Empty -> black
    translated = translate (positionToPixel x) (positionToPixel y)

drawBoard :: Board -> [Picture]
drawBoard Board {size = (sx, sy)} = makeLine sx id ++ makeLine sy swap
  where
    makeLine s f =
      map
        (\i -> line [f (positionToPixel i, -boardSizePixel / 2), f (positionToPixel i, boardSizePixel / 2)])
        [0 .. s - 1]

draw :: World -> Picture
draw (b, _) = pictures $ drawBoard b ++ concatMap drawStone ss
  where
    ss = stones b

inputHandler :: Event -> World -> World
inputHandler (EventKey (MouseButton LeftButton) Down _ (x', y')) (b, s) = nextWorld
  where
    x = pixelToPosition x'
    y = pixelToPosition y'
    maybeWorld = placeStone b s (x, y)
    newBoard = fromMaybe b maybeWorld
    otherMove = case s of
      Black -> White
      White -> Black
      Empty -> Black
    nextMove = if isJust maybeWorld then otherMove else s
    nextWorld = (newBoard, nextMove)
inputHandler _ b = b

update :: Float -> World -> World
update _ w = w

initialBoard :: Board
initialBoard = makeBoard boardSize

initialState :: World
initialState = (initialBoard, Black)

main :: IO ()
main = play windowDisplay (light orange) fps initialState draw inputHandler update
