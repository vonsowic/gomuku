module GameTree where

import Board as Board
import Data.Tree
import qualified Data.Map as GMap
import Position as Position
import Color as Color
import Evaluation (evaluate)
import Configuration
import Mark


newtype GameNode = GNode(Board, Mark)
board(GNode(b, _)) = b
mark(GNode(_, m)) = m

instance Show GameNode where
    show gn = show (GameTree.mark gn) ++ ":" ++ show (board gn)

------------------------------------------------------------------------------------------------------------------------
-- return list of boards, where each one of them contains diffrent next possible move
nextMoves board = possibleNextMoves (getMap board) (not' (getColor board))

canBeInserted m x y = GMap.lookup (Pos(x, y)) m == Nothing
possibleNextMoves m c = [insertCell m x y c | x <-Board.cords, y <- Board.cords, (canBeInserted m x y)]
------------------------------------------------------------------------------------------------------------------------

------ getters ---------------------------------------------------------------------------------------------------------
root(Node r _) = r
descendants(Node _ nodes) = nodes
childless tree = (length (descendants tree)) == 0
children tree = fmap (\child -> root child) $ descendants tree

childrenmarks tree = fmap (\child -> mark child) $ children tree

cut 1 tree = Node (root tree) []
cut n tree = Node (root tree) (fmap (\x->cut (n-1) x) $ descendants tree)

getChildByPos tree pos = head $ filter (\ch -> ((getPos (board (root ch))) == pos)) (descendants tree)
------------------------------------------------------------------------------------------------------------------------

----- Board with first move --------------------------------------------------------------------------------------------
fm = insertCell (GMap.fromList []) (succ (div boardsize 2)) (succ (div boardsize 2)) B
------------------------------------------------------------------------------------------------------------------------

------ create game tree using Data.Tree unfoldTree function ------------------------------------------------------------
gtree = unfoldTree gtreeFromSeed $ GNode(fm, evaluate fm)
gtreeFromSeed seed = (seed, fmap (\m -> GNode(m, evaluate m)) $ nextMoves $ board seed)
------------------------------------------------------------------------------------------------------------------------

----- for Data.Tree.Pretty ---------------------------------------------------------------------------------------------
getStringNode board = show board
------------------------------------------------------------------------------------------------------------------------

isterminal tree = Terminal == (mark $ root $ tree)