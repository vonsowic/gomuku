module Neighborhood where

import Data.List
import Position
import Board
import Color

------ n(neighbors) getters --------------------------------------------------------------------------------------------

nOnSlash board pos range = delete pos [Pos((x pos)+r, (y pos)+r) | r <- [-range..range]]
nOnSlashR board pos range = [Pos((x pos)+r, (y pos)-r) | r <- [1..range]]
nOnSlashL board pos range = [Pos((x pos)-r, (y pos)+r) | r <- [1..range]]

nOnBackSlash board pos range = delete pos [Pos((x pos)+r, (y pos)-r) | r <- [-range..range]]
nOnBackSlashR board pos range = [Pos((x pos)+r, (y pos)+r) | r <- [1..range]]
nOnBackSlashL board pos range = [Pos((x pos)-r, (y pos)-r) | r <- [1..range]]

nOnX board pos range = (nOnSlash board pos range) ++ (nOnBackSlash board pos range)

nInRow board pos range = (nInRowR board pos range) ++ (nInRowL board pos range)
nInRowR board pos range = [Pos(x', y pos) | x' <- [(x pos)+1..(x pos)+range]]
nInRowL board pos range = [Pos(x', y pos) | x' <- [(x pos)-range..(x pos)-1]]

nInCol board pos range = (nInColU board pos range) ++ (nInColD board pos range)
nInColU board pos range = [Pos(x pos, y') | y' <- [(y pos)-range..(y pos)-1]]
nInColD board pos range = [Pos(x pos, y') | y' <- [(y pos) + 1..(y pos)+range]]

nOfVonNeuman board pos range = (nInRow board pos range) ++ (nInCol board pos range)

nOfMoore board pos range = (nOfVonNeuman board pos range) ++ (nOnX board pos range)
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
friendly fun board pos range = [ pos' | pos' <- (fun board pos range), (getCell board pos' ) == Just (getColor board) ]
enemy fun board pos range = [ pos' | pos' <- (fun board pos range), (getCell board pos' ) == Just (not' (getColor board)) ]
------------------------------------------------------------------------------------------------------------------------