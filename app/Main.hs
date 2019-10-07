{-# OPTIONS -Wall #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Data.Tuple

import Board
import BoardUtils
import Data.Either (isRight, fromRight)

data World = World
  { currentBoard :: Board
  , currentTurn :: StoneType
  , lastBoards :: [Board]
  , redoBoards :: [Board]
}

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
initialState = World initialBoard Black [] []

-- (b, s, _, _)
draw :: World -> Picture
draw world = pictures $ drawBoard b ++ concatMap drawStone ss ++ [textTurn] ++ [instructions]
  where
    b = currentBoard world
    s = currentTurn world
    ss = stones b
    turn =
      case s of
        White -> "White"
        Black -> "Black"
        Empty -> "Invalid"
    textScale = scale 0.25 0.25
    pos = fromIntegral windowSize / 2
    textTurn = translate (-pos) (pos - fromIntegral padding / 1.5) $ textScale $ text turn
    instructions = translate (- fromIntegral (windowSize - padding) / 2) (-pos + fromIntegral padding / 3) $ textScale $ text "Z undo, X to redo, C to pass"

getOtherSymbol :: StoneType -> StoneType
getOtherSymbol s =
  case s of
    Black -> White
    White -> Black
    Empty -> Black


-- TODO is two pass, end of game
inputHandler :: Event -> World -> World
inputHandler (EventKey (MouseButton LeftButton) Down _ (x', y')) world = nextWorld
  where
    x = pixelToPosition x'
    y = pixelToPosition y'
    b = currentBoard world
    s = currentTurn world
    bs = lastBoards world
    afterPlay = placeStone b bs s (x, y)
    nextBoard = fromRight b afterPlay
    otherMove = getOtherSymbol s
    (nextMove, previousBoards) =
      if isRight afterPlay
        then (otherMove, b : bs)
        else (s, bs)
    nextWorld = World nextBoard nextMove previousBoards []
inputHandler (EventKey (Char 'z') Down _ _) world = nextWorld
  where
    s = currentTurn world
    b = currentBoard world
    redo = redoBoards world
    otherMove = getOtherSymbol s
    nextWorld = case lastBoards world of
                  p:bs -> World p otherMove bs (b:redo)
                  _ -> world
inputHandler (EventKey (Char 'x') Down _ _) world = nextWorld
  where
    s = currentTurn world
    b = currentBoard world
    bs = lastBoards world
    otherMove = getOtherSymbol s
    nextWorld = case redoBoards world of
                  p:redo -> World p otherMove (b:bs) redo
                  _ -> world
inputHandler (EventKey (Char 'c') Down _ _) world = nextWorld
  where
      s = currentTurn world
      b = currentBoard world
      bs = lastBoards world
      nextMove = getOtherSymbol s
      nextWorld = World b nextMove (b:bs) []
inputHandler _ b = b

update :: Float -> World -> World
update _ w = w

main :: IO ()
main = play windowDisplay (light orange) fps initialState draw inputHandler update