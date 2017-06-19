module Evaluation where

import Neighborhood
import Board
import Data.Map (fromList)
import Mark

----- main evaluation function -----------------------------------------------------------------------------------------

evaluate board = sum $ map (\eval -> (mark eval) * Mark((pattern eval) board)) evals
------------------------------------------------------------------------------------------------------------------------


----- evaluation type --------------------------------------------------------------------------------------------------
newtype EvaluationValue pattern = Eval(pattern, Mark)
pattern(Eval(p, _)) = p
mark(Eval(_, m)) = m
------------------------------------------------------------------------------------------------------------------------


----- patterns - return number of matches ------------------------------------------------------------------------------

-- there is one neighbor
one board = (onefr board) + (oneen board)

-- there is one friendly neighbor
onefr board = length $ friendly nOfMoore board (getPos board) 1

-- there is one enemy neighbor
oneen board = length $ enemy nOfMoore board (getPos board) 1
------------------------------------------------------------------------------------------------------------------------


------ evaluation values -----------------------------------------------------------------------------------------------
evals = [
    Eval(oneen, 1),
    Eval(onefr, 3)
    ] ++ [
    Eval(f, 25) | f <- (generate 2 friendly)
    ] ++ [
    Eval(f, 10) | f <- (generate 2 enemy)
    ] ++ [
    Eval(f, 250) | f <- (generate 3 friendly)
    ] ++ [
    Eval(f, 132) | f <- (generate 3 enemy)
    ]
    ++ (fmap (\f -> Eval(f, Terminal)) terminals)

------------------------------------------------------------------------------------------------------------------------


----- elementary neighborhoods - functions generator -------------------------------------------------------------------

-- generate patterns
generate range filter' = fmap (\n -> (\board -> (if (length (filter' n board (getPos board) range)) == range then 1 else 0))) elemns

orderedelemns = [
    (nOnSlashL, nOnSlashR),
    (nOnBackSlashL, nOnBackSlashR),
    (nInRowL, nInRowR),
    (nInColU, nInColD)
    ]

elemns = tupleToList orderedelemns

terminals = [
    (\board -> (if (((length (friendly (fst e) board (getPos board) (4-r))) == (4-r)) && ((length (friendly (snd e) board (getPos board) r)) == r))
        then 1
        else 0
        )) |
    r <- [1..4],
    e <- orderedelemns
    ] ++ [
    (\board -> (if (((length (friendly (snd e) board (getPos board) (4-r))) == (4-r)) && ((length (friendly (fst e) board (getPos board) r)) == r))
        then 1
        else 0
        )) |
    r <- [1..4],
    e <- orderedelemns
    ]
------------------------------------------------------------------------------------------------------------------------

tupleToList :: [(a,a)] -> [a]
tupleToList ((a,b):xs) = a : b : tupleToList xs
tupleToList _          = []
