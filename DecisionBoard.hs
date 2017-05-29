module DecisionBoard where

import Board as Board
import Data.Tree as Tree
import qualified Data.Map as Map
import Position as Position
import Color as Color


newtype GameNode = GNode(Board, Color, [GameNode])

instance Show (GameNode) where
    show x = show (getBoard x)

canBeInserted m x y = Map.lookup (Pos(x, y)) m == Nothing

possibleNextMoves m c = [insertCell m x y c | x <-Board.cords, y<-Board.cords, (canBeInserted m x y)]

-- return list of boards, where each one of them contains diffrent next possible move
nextMoves b c = possibleNextMoves (getMap b) c

firstMove = insertCell bMap 10 10  B

getBoard (GNode(b, _, _)) = b
getColor (GNode(_, c, _)) = c
getChildNodes (GNode(_, _, nodes)) = nodes

createTree board c = GNode (board, c, [ createTree childBoard (not' c) | childBoard <- (nextMoves board (not' c))])


--Tree.fmap do oceny
