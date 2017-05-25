module DecisionBoard where

import Board as Board
import Data.Tree as Tree
import qualified Data.Map as Map
import Position as Position
import Color as Color


data Game = DecisionBoard Board Color Position

newtype GameTree = Tree Game

newtype GameLeaf = Leaf(Board, [Board])

canBeInserted m x y = Map.lookup (Pos(x, y)) m == Nothing

possibleNextMoves m c = [insertCell m x y c | x <-Board.cords, y<-Board.cords, (canBeInserted m x y)]

firstMove = insertCell bMap 9 9 B
tree = Leaf(firstMove, (possibleNextMoves bMap W))


-- TODO: oceniajac drzewo nalezy uzyc fmap, dzieki czemu do kazdego elementu zostanie zapisana ocena
