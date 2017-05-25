module DecisionBoard where

import Board as Board
import Data.Tree as Tree
import qualified Data.Map as Map
import Position as Position
import Color as Color


--data Game = DecisionBoard Board Color Position
--newtype GameTree = Tree Game

newtype GameNode = GNode(Board, [GameNode], Color)


canBeInserted m x y = Map.lookup (Pos(x, y)) m == Nothing

possibleNextMoves m c = [insertCell m x y c | x <-Board.cords, y<-Board.cords, (canBeInserted m x y)]

firstMove = insertCell bMap 10 10  B

getColor (GNode(_, _, c)) = c
getBoard (GNode(b, _, c)) = b

nextMove b c = possibleNextMoves (getMap b) W
--createNode b c = GNode (b, nextMove b c, c)    

-- tree = createTree 

--Tree.fmap do oceny
