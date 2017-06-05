module Board where

import qualified Data.Map as GMap

import Data.List
import Color as Color
import Position as Position

newtype Board = Board(GMap.Map Position Color, Color)

instance Show (Board) where
    show x = "\n" ++ showBoard x

mapRows = 3
cords = [1..mapRows]

showBoard :: Board -> [Char]
showBoard board = intercalate "" [((showCell (getMap board) (Pos(x, y)))++nextRow y)| x <-cords, y<-cords]

showCell m pos
    | GMap.lookup pos m == Nothing = " . "
    | GMap.lookup pos m == Just B = " "++ (show B) ++" "
    | GMap.lookup pos m == Just W = " "++ (show W) ++" "

nextRow y
    | y /= mapRows = ""
    | y == mapRows = "\n"

getMap (Board(m, _)) = m
getColor (Board(_, c)) = c
getCell board pos = GMap.lookup pos (getMap board)

insertCellToMap m pos c = GMap.insert pos c m

insertCell m x y c = Board (insertCellToMap m (Pos(x, y)) c, c)
