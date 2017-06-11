module MinMax where

import GameTree (childrenmarks, descendants, board, root)
import Position
import Board(getPos)
import Data.List (elemIndex)


-- return next node
minmax gametree depth = descendants gametree !! (snd $ dig gametree ((depth `mod` 2) == 1) depth)

dig gametree maximize 1 = (getFunction maximize) $ zip (childrenmarks gametree) [0..]
dig gametree maximize depth =
    let fun = getFunction maximize
    in fun $ zip (fmap (\ch -> fst (dig ch (not maximize) (depth-1))) (descendants gametree)) [0..]

getFunction maximize = if maximize then maximum else minimum


a = 1
test =
    let tmp = a + 1
    in let a = tmp