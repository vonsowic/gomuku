module Color where

data Color = W|B deriving (Eq)

instance Show Color where
  show W = "o"  -- white
  show B = "x"  -- black

