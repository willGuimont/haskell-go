{-# OPTIONS -Wall #-}

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game

import           Data.Tuple

import           Board

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

initialGameState :: Board
initialGameState = makeBoard boardSize

positionToPixel :: Int -> Float
positionToPixel x = fromIntegral (x - halfBoardSize) * pixelPerCell

drawStone :: Stone -> [Picture]
drawStone Stone {stoneType = Empty} = [blank]
drawStone Stone {stoneType = s, position = (x, y)} =
  [ translate (positionToPixel x) (positionToPixel y) $ color c $ circleSolid stoneRadius
  , translate (positionToPixel x) (positionToPixel y) $ color black $ circle stoneRadius
  ]
  where
    c =
      case s of
        White -> white
        Black -> black
        Empty -> black

drawBoard :: Board -> [Picture]
drawBoard Board {size = (sx, sy)} = makeLine sx id ++ makeLine sy swap
  where
    makeLine s f =
      map
        (\i -> line [f (positionToPixel i, -boardSizePixel / 2), f (positionToPixel i, boardSizePixel / 2)])
        [0 .. s - 1]

draw :: Board -> Picture
draw b = pictures $ drawBoard b ++ concatMap drawStone ss
  where
    ss = stones b

inputHandler :: Event -> Board -> Board
inputHandler _ b = b

update :: Float -> Board -> Board
update _ b = b

main :: IO ()
main = play windowDisplay (light orange) fps initialGameState draw inputHandler update
