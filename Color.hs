module Color where

data Color = W|B deriving (Eq, Ord)

instance Show Color where
  show W = "o"  -- white
  show B = "x"  -- black

-- (<!>):: Color -> Color
-- (<!>) B = W
-- <!> W = B  

-- TODO: make ! operator
not':: Color -> Color
not' c 
	| c == W = B
	| c == B = W


