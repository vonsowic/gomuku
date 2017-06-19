import System.Environment()

import Position
import GameTree
import MinMax
import Configuration
import Board
import Mark

readPos = do
    input <- getLine
    let res = (read input :: Int)
    if (res > 0 && res <= boardsize)
        then return res
        else do
            print "Wrong format"
            readPos

-- computer vs computer
cvsc node = do
    let move = minmax node minmaxdepth
    putStrLn(show $ root move)
    if not $ isterminal move
        then cvsc move
        else do putStrLn("End of game: " ++ (show (getColor (board (root move)))) ++ " wins.\n")


-- computer vs human
cvsh node = do
    putStr("x: ")
    x <- readPos
    putStr("y: ")
    y <- readPos
    let humanmove = getChildByPos node (Pos(x, y))
    putStrLn(show $ root humanmove)
    if isterminal humanmove
        then putStrLn("You won.")
        else do
            let computermove = minmax humanmove minmaxdepth
            putStrLn(show $ root computermove)
            if isterminal computermove
                then putStrLn("Computer won.")
                else cvsh computermove

main = do
    putStrLn("Computer plays first.")
    putStrLn(show $ root gtree)
    --cvsc gtree
    cvsh gtree