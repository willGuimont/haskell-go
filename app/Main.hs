{-# OPTIONS -Wall #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Data.Maybe
import Data.Tuple

import Board
import BoardUtils

type World = (Board, StoneType, [Board], [Board])

-- Constants
fps :: Int
fps = 20

gameSize :: Int
gameSize = 9

halfBoardSize :: Int
halfBoardSize = 9 `div` 2

windowSize :: Int
windowSize = 550

padding :: Int
padding = 50

-- Helper functions
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

-- Graphics
drawStone :: Stone -> [Picture]
drawStone Stone {stoneType = Empty} = [blank]
drawStone Stone {stoneType = s, position = (x, y)} = [f col circleSolid, f black circle]
  where
    col =
      case s of
        White -> white
        Black -> black
        Empty -> black
    f c g = translate (positionToPixel x) (positionToPixel y) $ color c $ g stoneRadius

drawBoard :: Board -> [Picture]
drawBoard Board {size = (sx, sy)} = makeLine sx id ++ makeLine sy swap
  where
    makeLine s f =
      map
        (\i -> line [f (positionToPixel i, -boardSizePixel / 2), f (positionToPixel i, boardSizePixel / 2)])
        [0 .. s - 1]

-- Game
initialBoard :: Board
initialBoard = makeBoard boardSize

initialState :: World
initialState = (initialBoard, Black, [], [])

draw :: World -> Picture
draw (b, s, _, _) = pictures $ drawBoard b ++ concatMap drawStone ss ++ [textTurn] ++ [instructions]
  where
    ss = stones b
    turn =
      case s of
        White -> "White"
        Black -> "Black"
        Empty -> "Invalid"
    textScale = scale 0.25 0.25
    pos = fromIntegral windowSize / 2
    textTurn = translate (-pos) (pos - fromIntegral padding / 2) $ textScale $ text turn
    instructions = translate (- fromIntegral (windowSize - padding) / 2) (-pos + fromIntegral padding / 2) $ textScale $ text "Z undo, X to redo"

getOtherSymbol :: StoneType -> StoneType
getOtherSymbol s =
  case s of
    Black -> White
    White -> Black
    Empty -> Black

-- TODO space to pass
-- TODO is two pass, end of game
inputHandler :: Event -> World -> World
inputHandler (EventKey (MouseButton LeftButton) Down _ (x', y')) (b, s, bs, _) = nextWorld
  where
    x = pixelToPosition x'
    y = pixelToPosition y'
    maybeWorld = placeStone b bs s (x, y)
    nextBoard = fromMaybe b maybeWorld
    otherMove = getOtherSymbol s
    (nextMove, previousBoard) =
      if isJust maybeWorld
        then (otherMove, b : bs)
        else (s, bs)
    nextWorld = (nextBoard, nextMove, previousBoard, [])
inputHandler (EventKey (Char 'z') Down _ _) (b, s, p:bs, redo) = (p, otherMove, bs, b : redo)
  where
    otherMove = getOtherSymbol s
inputHandler (EventKey (Char 'x') Down _ _) (b, s, bs, p:redo) = (p, otherMove, b:bs,  redo)
  where
    otherMove = getOtherSymbol s
inputHandler _ b = b

update :: Float -> World -> World
update _ w = w

main :: IO ()
main = play windowDisplay (light orange) fps initialState draw inputHandler update