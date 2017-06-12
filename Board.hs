module Board where

import qualified Data.Map as GMap

import Data.List
import Color
import Position
import Configuration

newtype Board = Board(GMap.Map Position Color, Position, Color)

instance Show (Board) where
    show x = "\n" ++ showBoard x

cords = [1..boardsize]

showBoard :: Board -> [Char]
showBoard board =
    (rowIndexes boardsize)
    ++ "\n" ++
    (intercalate "" [((showCell (getMap board) (Pos(x, y)))++nextRow x y)| y<-cords, x <-cords])

showCell m pos
    | GMap.lookup pos m == Nothing = " . "
    | GMap.lookup pos m == Just B = " "++ (show B) ++" "
    | GMap.lookup pos m == Just W = " "++ (show W) ++" "

nextRow x y
    | x /= boardsize = ""
    | x == boardsize = " " ++ (show y) ++ "\n"

getMap (Board(m, _, _)) = m
getColor (Board(_, _, c)) = c
getPos(Board(_, p, _)) = p

getCell board pos = GMap.lookup pos (getMap board)
insertCellToMap m pos c = GMap.insert pos c m

insertCell m x y c = Board (insertCellToMap m (Pos(x, y)) c, Pos(x, y), c)
------------------------------------------------------------------------------------------------------------------------

rowIndexes 1 = " 1"
rowIndexes n = (rowIndexes (n-1)) ++ (take (2 - (div n 10)) (repeat ' ')) ++ (show n)
-- 1 2 3 4 5
-- 2 . . . .
-- 3 . . x .
-- 4 . o . .
-- 5 . . . .
