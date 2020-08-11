module Pos where

import qualified Prelude as P

infixl 6 |+, |-, >+, >-
infixl 7 |*, >*

type Pos = (P.Int, P.Int)
type Pos2 = Pos -> Pos
type PosExp = Pos -> [Pos]
type LPos = [Pos] -> [Pos]

(|+) :: Pos -> Pos2
(|-) :: Pos -> Pos2
(|*) :: P.Int -> Pos2

(|+) (x,y) (dx,dy) = (x P.+ dx, y P.+ dy)
(|-) (x,y) (dx,dy) = (x P.- dx, y P.- dy)
(|*) i (x,y) = (i P.* x, i P.* y)


(>+) :: Pos -> LPos
(>-) :: Pos -> LPos
(>*) :: P.Int ->LPos

(>+) p = P.map (p |+)
(>-) p = P.map (p |-)
(>*) i = P.map (i |*)
