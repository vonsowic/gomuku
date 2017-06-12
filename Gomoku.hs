import System.Environment()

import Position
import GameTree
import MinMax
import Configuration


readPos = do
    input <- getLine
    let res = (read input :: Int)
    if (res > 0 && res <= boardsize)
        then return res
        else do
            print "Wrong format"
            readPos

move node = do
    --x <- readPos
    -- y <- readPos
    --let humanmove = getChildByPos node (Pos(x, y))
    let humanmove = minmax node minmaxdepth
    putStrLn(show $ root humanmove)
    let computermove = minmax humanmove minmaxdepth
    putStrLn(show $ root computermove)
    move computermove

main = do
    putStrLn(show $ root gtree)
    move gtree