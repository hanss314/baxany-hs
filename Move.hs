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

rawGetMoves :: Board -> Piece -> Pos -> [Move]
-- Pawn
rawGetMoves b pawn@(Piece c (Pawn m)) p = map listify $ forward ++ second ++ captures  ++ eps where
    forward = filter ((==) Empty . getPiece b) $ map ((p |+) . mpb c) [(0,1)]
    second = if null forward || m /= Start then [] else 
            filter ((==) Empty . getPiece b) $ map ((p |+) . mpb c) [(0,2)]
    captures = filter (canCapture pawn . getPiece b) $ map ((p |+) . mpb c) [(1,1), (-1,1)]
    eps = filter (not . null . getEps c b) $ map ((p |+) . mpb c) [(1,1), (-1,1)]

-- Rest of normal chess pieces
rawGetMoves b king@(Piece _ King) p = basicFilter b king $ p >+ ua
rawGetMoves b knight@(Piece _ Knight) p = basicFilter b knight $ p >+ (mh (1,2) >>= r4)
rawGetMoves b bishop@(Piece _ Bishop) p = basicFilterSlider b bishop p ud
rawGetMoves b rook@(Piece _ Rook) p = basicFilterSlider b rook p uo
rawGetMoves b queen@(Piece _ Queen) p = basicFilterSlider b queen p ua

-- Knight compounds
rawGetMoves b (Piece c Amazon) p = (rawGetMoves b (Piece c Queen) p) ++ (rawGetMoves b (Piece c Knight) p)
rawGetMoves b (Piece c Cardinal) p = (rawGetMoves b (Piece c Bishop) p) ++ (rawGetMoves b (Piece c Knight) p)
rawGetMoves b (Piece c Chancellor) p = (rawGetMoves b (Piece c Rook) p) ++ (rawGetMoves b (Piece c Knight) p)
rawGetMoves b pie@(Piece _ Knightmare) p = basicFilterSlider b pie p $ mh (1,2) >>= r4
rawGetMoves b (Piece c Mage) p = rawGetMoves b (Piece c Knight) p

-- Double movers
rawGetMoves b pie@(Piece _ HookMover) p = doubleMoverN b pie p ud
rawGetMoves b pie@(Piece _ Empress) p = doubleMoverN b pie p ua

-- Cannons
rawGetMoves b pie@(Piece _ Pao) p = map listify $ uo >>= cannon b pie p
rawGetMoves b pie@(Piece _ Vao) p = map listify $ ud >>= cannon b pie p
rawGetMoves b pie@(Piece _ Leo) p = map listify $ ua >>= cannon b pie p

-- Knightlikes
rawGetMoves b pie@(Piece c Cobra) p = basicFilter b pie $ p >+ r4 (2,2)
rawGetMoves b pie@(Piece c Camel) p = basicFilter b pie $ p >+ (r4 (3,1) >>= mh)
rawGetMoves b pie@(Piece c Zebra) p = basicFilter b pie $ p >+ (r4 (3,2) >>= mh)
rawGetMoves b pie@(Piece c Giraffe) p = basicFilter b pie $ p >+ (r4 (4,2) >>= mh)
rawGetMoves b pie@(Piece c Kangaroo) p = basicFilter b pie $ p >+ (r4 (4,3) >>= mh)

-- Other special ones
rawGetMoves b pie@(Piece c General) p = rawGetMoves b (Piece c King) p ++ swaps
    where
        swaps = map listify $ filter ((==) (Piece c King) . getPiece b) $ p >+ [(x,y) | x<-[-2..2], y<-[-2..2]]

rawGetMoves b pie@(Piece c Lance) p = basicFilterSlider b pie p $ [mpb c (0,1)]



-- default piece has no moves
rawGetMoves _ _ _ = []

getMoves :: Board -> Piece -> Pos -> [Move]
getMoves board piece@(Piece c _) pos = rawMoves ++ if hasMage then mageBoost else [] where
    rawMoves = rawGetMoves board piece pos
    hasMage = elem (Piece c Mage) $ map (getPiece board) $ pos >+ ua
    mageBoost = (map MageMove . filter ((\p -> (p == Empty || canCapture piece p)) . getPiece board) 
                              . filter (not . flip elem rawMoves . listify)) $ pos >+ (mh (1,2) >>= r4)
    
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
doMove b pawn@(Piece c (Pawn _)) s@(x,y) (N m@[e@(mx,my)])
    | length epsPos > 0 = b & preCapture epCapPie epCap s & normalmove & postCapture epCapPie epCap
    | x /= mx = capturingMove s e b
    | otherwise = normalmove b
    where
        epsPos = getEps c b e
        epCap = head epsPos
        epCapPie = getPiece b epCap
        myeps = [(x,j) | j <- [(min y my)+1..(max y my)-1]]
        normalmove = rawSetPieces $ (e,nextPawn pawn myeps) : (s,Empty) :  map (flip (,) Empty) epsPos

doMove b (Piece c Lance) s (N [e@(x,y)]) = promotion end
    where
        end = normalMove s e b
        target = if c == Black then 0 else (size b) - 1
        promotion = if y == target then rawSetPiece e (Piece c Rook) else id

doMove b pie@(Piece c General) s (N [e]) = if target /= (Piece c King) then normalMove s e b else
    rawSetPieces [(s, target), (e, pie)] b
    where 
        target = getPiece b e
    

doMove b _ s (N [e]) = normalMove s e b
doMove b _ s (MageMove e) = normalMove s e b
doMove b _ _ _ = b


