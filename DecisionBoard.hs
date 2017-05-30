module DecisionBoard where

import Board as Board
import Data.Tree as Tree
import qualified Data.Map as GMap
import Position as Position
import Color as Color


newtype GameNode = GNode(Board, [GameNode])

instance Show (GameNode) where
    show x = show (getBoard x)

canBeInserted m x y = GMap.lookup (Pos(x, y)) m == Nothing

possibleNextMoves m c = [insertCell m x y c | x <-Board.cords, y<-Board.cords, (canBeInserted m x y)]

-- return list of boards, where each one of them contains diffrent next possible move
nextMoves b = possibleNextMoves (getMap b) (not' (getColor b))

-- Board with first move
firstMove = insertCell (GMap.fromList []) 10 10 B

getBoard (GNode(b, _)) = b
getNodes (GNode(_, nodes)) = nodes

getNode node [a] = (getNodes node) !! a
getNode node indexes = getNode ((getNodes node) !! (head indexes)) (tail indexes)

createNodes board = [ childBoard | childBoard <- (nextMoves board )]

createTree board = GNode (board, [createTree node | node <- createNodes board ])

-- return [Position] next to Position(x, y)
neighbors board x y = [ Pos(x', y') | x' <- [x-1..x+1], y' <- [y-1..y+1], x /= x' || y /= y']
-- return [Position] next to Position(x, y) which colors differ from Board's Color
enemyNeighbors board x y = [ pos | pos <- (neighbors board x y), (getCell board pos ) == Just (not' (getColor board)) ]
-- return [Position] next to Position(x, y) which colors matches Board's Color
friendlyNeighbors board x y = [ pos | pos <- (neighbors board x y), (getCell board pos ) == Just (getColor board) ]




--Tree.fmap do oceny