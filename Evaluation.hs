module Evaluation where

import DecisionBoard
import Board

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
one board = length $ nOfMoore board (getPos board) 1

-- there is one friendly neighbor
onefr board = length $ friendly nOfMoore board (getPos board) 1


------------------------------------------------------------------------------------------------------------------------


------ evaluation values -----------------------------------------------------------------------------------------------
evals = [
    Eval(one, 1),
    Eval(onefr, 5)
    ]
------------------------------------------------------------------------------------------------------------------------
