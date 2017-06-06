module Position where

newtype Position = Pos(Int, Int) deriving (Show, Eq)

instance Ord Position where
    compare (Pos(l1,r1)) (Pos(l2,r2))
        | l1 == l2 && r1==r2 = EQ
        | l1 == l2 && r1<r2 = LT
        | l1 == l2 && r1>r2 = GT
        | l1 < l2 = LT
        | l1 > l2 = GT

instance Num Position where
    (Pos (x1, y1)) + (Pos(x2, y2)) = Pos(x1+x2, y1+y2)
    (Pos (x1, y1)) - (Pos(x2, y2)) = Pos(x1-x2, y1-y2)


x(Pos(x, _)) = x
y(Pos(_, y)) = y

abs':: Int -> Int
abs' n = if n >= 0 then n else (-1) * n

isNextTo pos1 pos2 = abs ((x pos1) - (x pos2)) <=1 && abs ((y pos1) - (y pos2)) <=1

tuple (Pos(x, y)) = (x, y)