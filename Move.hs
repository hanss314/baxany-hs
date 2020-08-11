module Move where

import Pos
import Piece
import Board
import Transforms
import MoveHelper
import Hooks
import Interactions

import Data.Function
import Data.List

getEps :: Color -> Board -> Pos -> [Pos]
getEps c board pos@(x,y) = filter (canEp pos . getPiece board) $ map ((,) x) [0..(size board)-1] where
    canEp movepos (Piece pc (Pawn (EnPassant eppos))) = c /= pc && elem movepos eppos
    canEp _ _ = False

getMoves :: Board -> Piece -> Pos -> [Move]
-- Pawn
getMoves b pawn@(Piece c (Pawn m)) p = forward ++ second ++ captures ++ eps where
    forward = filter ((==) Empty . getPiece b . head) $ map ((p >+) . mb c) [[(0,1)]]
    second = if null forward || m /= Start then [] else 
            filter ((==) Empty . getPiece b . head) $ map ((p >+) . mb c) [[(0,2)]]
    captures = filter (canCapture pawn . getPiece b . head) $ map ((p >+) . mb c) [[(1,1)], [(-1,1)]]
    eps = filter (not . null . getEps c b . head) $ map ((p >+) . mb c) [[(1,1)], [(-1,1)]]

-- Rest of normal chess pieces
getMoves b king@(Piece _ King) p = basicFilter b king $ p >+ ua
getMoves b knight@(Piece _ Knight) p = basicFilter b knight $ p >+ (mh (1,2) >>= r4)
getMoves b bishop@(Piece _ Bishop) p = basicFilterSlider b bishop p ud
getMoves b rook@(Piece _ Rook) p = basicFilterSlider b rook p uo
getMoves b queen@(Piece _ Queen) p = basicFilterSlider b queen p ua

-- Knight compounds
getMoves b (Piece c Amazon) p = (getMoves b (Piece c Queen) p) ++ (getMoves b (Piece c Knight) p)
getMoves b (Piece c Cardinal) p = (getMoves b (Piece c Bishop) p) ++ (getMoves b (Piece c Knight) p)
getMoves b (Piece c Chancellor) p = (getMoves b (Piece c Rook) p) ++ (getMoves b (Piece c Knight) p)
getMoves b pie@(Piece _ Knightmare) p = basicFilterSlider b pie p $ mh (1,2) >>= r4

-- Double movers
getMoves b pie@(Piece _ HookMover) p = doubleMoverN b pie p ud

getMoves b pie@(Piece c Lance) p = basicFilterSlider b pie p $ mb c [(0,1)]


-- default piece has no moves
getMoves _ _ _ = []

movesAt :: Board -> Pos -> [Move]
movesAt b pos = getMoves b (getPiece b pos) pos

capturingMove :: Pos -> Pos -> Board -> Board
capturingMove s e b = b & preCapture p e s & rawMovePiece s e & postCapture p e where
    p = getPiece b e

doMoveAt :: Board -> Pos -> Move -> Board
doMoveAt b pos move = doMove b (getPiece b pos) pos move

-- Assume Move is legal here
doMove :: Board -> Piece -> Pos -> Move -> Board

-- Pawn
doMove b pawn@(Piece c (Pawn _)) s@(x,y) m@[e@(mx,my)]
    | length epsPos > 0 = b & preCapture epCapPie epCap s & normalmove & postCapture epCapPie epCap
    | x /= mx = capturingMove s e b
    | otherwise = normalmove b
    where
        epsPos = getEps c b e
        epCap = head epsPos
        epCapPie = getPiece b epCap
        myeps = [(x,j) | j <- [(min y my)+1..(max y my)-1]]
        normalmove = rawSetPieces $ (e,nextPawn pawn myeps) : (s,Empty) :  map (flip (,) Empty) epsPos

doMove b (Piece c Lance) s [e@(x,y)] = promotion end
    where
        end = normalMove s e b
        target = if c == Black then 0 else (size b) - 1
        promotion = if y == target then rawSetPiece e (Piece c Rook) else id

doMove b _ s [e] = normalMove s e b
doMove b _ _ _ = b


