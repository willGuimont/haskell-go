module Board
    (
    ) where

data Tile = Black | White | Empty
type Row = [Tile]
type Board = [Row]
type Position = (Int, Int)
type Size = (Int, Int)

makeBoard :: Size -> Board
makeBoard s = replicate y $ replicate x Empty
  where x = fst s
        y = snd s

getRow :: Board -> Int -> Maybe Row
getRow = getNth

getTile :: Board -> Position -> Maybe Tile
getTile board pos = getRow board y >>= \r -> getNth r x
  where x = fst pos
        y = snd pos

play :: Board -> Tile -> Position -> Either () Board
play board tile position =
  case newBoard of
    Nothing -> Left ()
    Just b -> Right b
  where
    x = fst position
    y = snd position
    oldRow = getRow board y
    fMapped = fmap (\r -> replace r x tile)
    newRow = fMapped oldRow
    newBoard = undefined

getNth :: [a] -> Int -> Maybe a
getNth [] 0 = Nothing
getNth xs 0 = Just (head xs)
getNth xs n = getNth (tail xs) (n - 1)

replace :: [a] -> Int -> a -> [a]
replace xs n x = start ++ [x] ++ rest
  where start = take n xs
        rest = drop (n + 1) xs
