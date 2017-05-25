module Board where

import qualified Data.Map as Map

import Data.List
import Color as Color
import Position as Position

newtype Board = Board(Map.Map Position Color)

instance Show (Board) where
    show x = showBoard x

mapRows = 19
cords = [1..mapRows]

bMap = Map.fromList []

board = Board bMap

showBoard :: (Board) -> [Char]
showBoard (Board m) = intercalate "" [((showCell m (Pos(x, y)))++nextRow y)| x <-cords, y<-cords]

showCell m pos
    | Map.lookup pos m == Nothing = " . "
    | Map.lookup pos m == Just B = " "++ (show B) ++" "
    | Map.lookup pos m == Just W = " "++ (show W) ++" "

nextRow y
    | y /= mapRows = ""
    | y == mapRows = "\n"

getMap (Board(m)) = m 

--getColor (Board(_ _ c)) = c

insertCellToMap m pos c = Map.insert pos c m

insertCell m x y c = Board (insertCellToMap m (Pos(x, y)) c)
