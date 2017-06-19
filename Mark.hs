module Mark where

------ Mark type, which is Int or Terminal(Infinity) -------------------------------------------------------------------
data Mark = Mark Int | Terminal deriving (Eq, Ord, Show)

instance Num Mark where
    Mark(x1) + Mark(x2) = Mark(x1+x2)
    Terminal + Mark(x1) = Terminal
    Mark(x1) + Terminal = Terminal
    Terminal + Terminal = Terminal

    Mark(x1) - Mark(x2) = Mark(x1-x2)
    Terminal - Mark(x1) = Terminal
    Mark(x1) - Terminal = Mark 0

    Mark(x1) * Mark(x2) = Mark(x1*x2)
    Terminal * Mark(0) = Mark 0
    Terminal * Mark(x1) = Terminal
    Terminal * Terminal = Terminal
    Mark(0)  * Terminal = Mark 0
    Mark(x1) * Terminal = Terminal

    fromInteger 2147483647 = Terminal
    fromInteger x = Mark(fromInteger x)

    signum Terminal = 1
    signum (Mark x) = Mark(signum x)

    abs Terminal = Terminal
    abs (Mark x) = Mark(abs x)
------------------------------------------------------------------------------------------------------------------------