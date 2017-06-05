module DecisionBoard where

import Board as Board
import Data.Tree as Tree
import qualified Data.Map as GMap
import Position as Position
import Color as Color
import Data.Tree.Pretty as P


------ type representing tree node -------------------------------------------------------------------------------------
newtype GameNode = GNode(Board, [GameNode])

instance Show (GameNode) where
    show x = show (getBoard x)

------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
canBeInserted m x y = GMap.lookup (Pos(x, y)) m == Nothing
possibleNextMoves m c = [insertCell m x y c | x <-Board.cords, y<-Board.cords, (canBeInserted m x y)]
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- return list of boards, where each one of them contains diffrent next possible move
nextMoves b = possibleNextMoves (getMap b) (not' (getColor b))
------------------------------------------------------------------------------------------------------------------------


----- Board with first move --------------------------------------------------------------------------------------------
firstMove = insertCell (GMap.fromList []) 2 2 B
------------------------------------------------------------------------------------------------------------------------


------ getters for GameNode --------------------------------------------------------------------------------------------
getBoard (GNode(b, _)) = b
getNodes (GNode(_, nodes)) = nodes
------------------------------------------------------------------------------------------------------------------------


------ getters for custom tree -----------------------------------------------------------------------------------------
getNode node [a] = (getNodes node) !! a
getNode node indexes = getNode ((getNodes node) !! (head indexes)) (tail indexes)

-- custom tree creater
createTree board = GNode (board, [createTree node | node <- createNodes board ])
------------------------------------------------------------------------------------------------------------------------


------ create tree using Data.Tree unfoldTree function -----------------------------------------------------------------
createNodes board = [ childBoard | childBoard <- (nextMoves board )]
createTreeNode seed = (seed, createNodes seed)
plantTree = Tree.unfoldTree (createTreeNode) firstMove
------------------------------------------------------------------------------------------------------------------------


-- for Data.Tree.Pretty
getStringNode board = show board
------------------------------------------------------------------------------------------------------------------------


------ neighbors getters -----------------------------------------------------------------------------------------------
neighborsOnX board x y = [Pos(x', y') | x' <- [x-1..x+1], y' <- [y-1..y+1], x /= x' && y /= y']

neighborsOfVonNeuman board x y = [
        Pos(x', y') | x' <- [x-1..x+1], y' <- [y-1..y+1], x == x' || y == y', not(x == x' && y == y')
    ]

neighborsOfMoore board x y = (neighborsOfVonNeuman board x y ) ++ (neighborsOnX board x y )

friendlyNeighbors fun board x y = [ pos | pos <- (fun board x y), (getCell board pos ) == Just (getColor board) ]
enemyNeighbors fun board x y = [ pos | pos <- (fun board x y), (getCell board pos ) == Just (not' (getColor board)) ]
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- get all positions from board, where posotion color matches color param
friendlyPositions board color = map fst (filter (\m -> (snd m) == color) (GMap.toList ( getMap firstMove)))
------------------------------------------------------------------------------------------------------------------------























