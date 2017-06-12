module Neighborhood where

import Data.List
import Position
import Board
import Color
import Configuration

------ n(neighbors) getters --------------------------------------------------------------------------------------------

nOnSlash board pos range = (nOnSlashR board pos range) ++ (nOnSlashL board pos range)
nOnSlashR board pos range = [Pos((x pos)+r, (y pos)-r) | r <- [1..minimum [range, boardsize-(x pos), boardsize-(y pos)]]]
nOnSlashL board pos range = [Pos((x pos)-r, (y pos)+r) | r <- [1..minimum [range, (x pos)-1, (y pos)-1]]]

nOnBackSlash board pos range  = (nOnBackSlashR board pos range) ++ (nOnBackSlashL board pos range)
nOnBackSlashR board pos range = [Pos((x pos)+r, (y pos)+r) | r <- [1..minimum [range, boardsize-(x pos), boardsize-(y pos)]]]
nOnBackSlashL board pos range = [Pos((x pos)-r, (y pos)-r) | r <- [1..minimum [range, (x pos)-1, (y pos)-1]]]

nOnX board pos range = (nOnSlash board pos range) ++ (nOnBackSlash board pos range)

nInRow board pos range = (nInRowR board pos range) ++ (nInRowL board pos range)
nInRowR board pos range = [Pos(x', y pos) | x' <- [(x pos)+1..(min ((x pos)+range) boardsize)]]
nInRowL board pos range = [Pos(x', y pos) | x' <- [(max ((x pos)-range) 1)..(x pos)-1]]

nInCol board pos range = (nInColU board pos range) ++ (nInColD board pos range)
nInColU board pos range = [Pos(x pos, y') | y' <- [(max ((y pos)-range) 1)..(y pos)-1]]
nInColD board pos range = [Pos(x pos, y') | y' <- [(y pos) + 1..(min ((y pos)+range) boardsize)]]

nOfVonNeuman board pos range = (nInRow board pos range) ++ (nInCol board pos range)

nOfMoore board pos range = (nOfVonNeuman board pos range) ++ (nOnX board pos range)
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
friendly fun board pos range = [ pos' | pos' <- (fun board pos range), (getCell board pos' ) == Just (getColor board) ]
enemy fun board pos range = [ pos' | pos' <- (fun board pos range), (getCell board pos' ) == Just (not' (getColor board)) ]
------------------------------------------------------------------------------------------------------------------------
