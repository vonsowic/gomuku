module DecisionBoard where

import Board as Board
import Data.Tree as Tree


data Game = DecisionBoard Board Color Pos

newtype GameTree = Tree Game



--TODO: oceniajac drzewo nalezy uzyc fmap, dzieki czemu do kazdego elementu zostanie zapisana ocena
