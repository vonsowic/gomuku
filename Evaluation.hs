module Evaluation where

import Neighborhood
import Board
import Data.Map (fromList)

----- main evaluation function -----------------------------------------------------------------------------------------

evaluate board = sum $ map (\eval -> (mark eval) * ((pattern eval) board)) evals
------------------------------------------------------------------------------------------------------------------------

----- evaluation type --------------------------------------------------------------------------------------------------
newtype EvaluationValue pattern = Eval(pattern, Int)
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
    Eval(one, 1),
    Eval(onefr, 3)
    ] ++ [
    Eval(f, 25) | f <- (generate 2 friendly)
    ] ++ [
    Eval(f, 10) | f <- (generate 2 enemy)
    ] ++ [
    Eval(f, 50) | f <- (generate 3 friendly)
    ] ++ [
    Eval(f, 32) | f <- (generate 3 enemy)
    ] ++ [
    Eval(f, 1000) | f <- (generate 4 friendly)
    ] ++ [
    Eval(f, 490) | f <- (generate 4 enemy)
    ]
------------------------------------------------------------------------------------------------------------------------


----- elementary neighborhoods - functions generator -------------------------------------------------------------------

-- generate patterns
generate range filter' = fmap (\n -> (\board -> (if (length (filter' n board (getPos board) range)) == range then 1 else 0))) elemns

elemns = [
    nOnSlashR,
    nOnSlashL,
    nOnBackSlashL,
    nOnBackSlashR,
    nInRowL,
    nInRowR,
    nInColU,
    nInColD
    ]
------------------------------------------------------------------------------------------------------------------------

isterminal board = True `elem` [(f board) == 4 | f <- (generate 4 friendly)]
