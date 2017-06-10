module DecisionBoard where

import Board as Board
import Data.Tree as Tree
import Data.List as List
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

--getChildNodes(Tree.Node(_, children)) = children

------------------------------------------------------------------------------------------------------------------------


----- for Data.Tree.Pretty ---------------------------------------------------------------------------------------------
getStringNode board = show board
------------------------------------------------------------------------------------------------------------------------


------ n(neighbors) getters --------------------------------------------------------------------------------------------

nOnSlash board pos range = List.delete pos [Pos((x pos)+r, (y pos)+r) | r <- [-range..range]]
nOnBackSlash board pos range = List.delete pos [Pos((x pos)+r, (y pos)-r) | r <- [-range..range]]
nOnX board pos range = (nOnSlash board pos range) ++ (nOnBackSlash board pos range)

nInRow board pos range = List.delete pos [Pos(x', y pos) | x' <- [(x pos)-range..(x pos)+range]]
nInCol board pos range = List.delete pos [Pos(x pos, y') | y' <- [(y pos)-range..(y pos)+range]]
nOfVonNeuman board pos range = (nInRow board pos range) ++ (nInCol board pos range)

nOfMoore board pos range = (nOfVonNeuman board pos range) ++ (nOnX board pos range)

friendly fun board pos range = [ pos' | pos' <- (fun board pos range), (getCell board pos' ) == Just (getColor board) ]
enemy fun board pos range = [ pos' | pos' <- (fun board pos range), (getCell board pos' ) == Just (not' (getColor board)) ]
------------------------------------------------------------------------------------------------------------------------


----- get all positions from board, where position's color matches param color -----------------------------------------
positionsOfColor board color = map fst (filter (\m -> (snd m) == color) (GMap.toList (getMap firstMove)))
------------------------------------------------------------------------------------------------------------------------