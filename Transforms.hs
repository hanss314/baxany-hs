module Transforms where

import Pos

-- reflectors (mirrors)
mv :: PosExp
mh :: PosExp
ma :: PosExp
mv p@(x,y) = [p, (-x,y)]
mh p@(x,y) = [p, (x,-y)]
ma p = mv p >>= mh

-- rotators
rr :: Pos2
rrn :: Int -> Pos2
rr (x,y) = (y,-x) 
rrn n p = iterate rr p !! n

rl :: Pos2
rln :: Int -> Pos2
rl (x,y) = (-y,x)
rln n p = iterate rl p !! n

r4 :: PosExp
r4 = (take 4 . iterate rr)

--basic units
uo = r4 (0,1)
ud = ma (1,1)
ua = uo ++ ud
f1 = (0,1)

ss :: Pos2
ss (x,y) = (x,-y)
mss = map ss

