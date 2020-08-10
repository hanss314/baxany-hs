module Move where

import Pos
import Piece
import Board
import Transforms

import Data.Function

type Move = [Pos]

mb :: Color -> Move -> Move
mb White = id
mb Black = map (\(x,y)->(x,-y)) 

mmb :: Color -> [Move] -> [Move]
mmb c = map (mb c)

listify :: a -> [a]
listify x = [x]

-- capturer capturee
canCapture :: Piece -> Piece -> Bool
canCapture (Piece c _) (Piece b _) = c /= b
canCapture _ Empty = True
canCapture _ _ = False

-- capturee capturee capturer
preCapture :: Piece -> Pos -> Pos -> Board -> Board
preCapture _ _ _ b = b

postCapture :: Piece -> Pos -> Board -> Board
postCapture _ _ b = b

getEps :: Color -> Board -> Pos -> [Pos]
getEps c board pos@(x,y) = filter (canEp pos . getPiece board) $ map ((,) x) [0..(size board)-1] where
    canEp movepos (Piece pc (Pawn (EnPassant eppos))) = c /= pc && elem movepos eppos
    canEp _ _ = False

basicFilter :: Board -> Piece -> [Pos] -> [Move]
basicFilter b p = (map listify . filter (canCapture p . getPiece b))

basicSlider :: Board -> Piece -> Pos -> Pos -> [Pos]
basicSlider board piece start step
    | nextPiece == Empty = nextPos : basicSlider board piece nextPos step
    | canCapture piece nextPiece = [nextPos]
    | otherwise = []
    where 
        nextPos = start |+ step
        nextPiece = getPiece board nextPos

basicFilterSlider :: Board -> Piece -> Pos -> [Pos] -> [Move]
basicFilterSlider board piece pos steps = basicFilter board piece $ steps >>= basicSlider board piece pos

getMoves :: Board -> Piece -> Pos -> [Move]
-- Pawn
getMoves b pawn@(Piece c (Pawn m)) p = forward ++ second ++ captures ++ eps where
    forward = filter ((==) Empty . getPiece b . head) $ map ((p >+) . mb c) [[(0,1)]]
    second = if null forward || m /= Start then [] else 
            filter ((==) Empty . getPiece b . head) $ map ((p >+) . mb c) [[(0,2)]]
    captures = filter (canCapture pawn . getPiece b . head) $ map ((p >+) . mb c) [[(1,1)], [(-1,1)]]
    eps = filter (not . null . getEps c b . head) $ map ((p >+) . mb c) [[(1,1)], [(-1,1)]]

-- Rest of normal chess pieces
getMoves b king@(Piece _ King) p = basicFilter b king $ p >+ ([(0,1),(1,1)] >>= r4)
getMoves b knight@(Piece _ Knight) p = basicFilter b knight $ p >+ (mh (1,2) >>= r4)
getMoves b bishop@(Piece _ Bishop) p = basicFilterSlider board bishop p $ r4 (1,1)
getMoves b rook@(Piece _ Rook) p = basicFilterSlider board rook p $ r4 (1,0)
getMoves b queen@(Piece _ Queen) p = basicFilterSlider board rook p $ [(0,1), (1,1)] >>= r4

-- default piece has no moves
getMoves _ _ _ = []

movesAt :: Board -> Pos -> [Move]
movesAt b pos = getMoves b (getPiece b pos) pos

capturingMove :: Pos -> Pos -> Board -> Board
capturingMove s e b = b & preCapture p e s & rawMovePiece s e & postCapture p e where
    p = getPiece b e

normalMove :: Pos -> Pos -> Board -> Board
normalMove start end b = case getPiece b end of
    Block -> b
    Empty -> rawMovePiece start end b
    x -> b & preCapture x end start & rawMovePiece start end & postCapture x end

nextPawn :: Piece -> [Pos] -> Piece
nextPawn (Piece c (Pawn Start)) eps = (Piece c (Pawn (EnPassant eps)))
nextPawn p _ = p

-- Assume Move is legal here
doMove :: Board -> Piece -> Pos -> Move -> Board
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

-- Regular chess pieces
doMove b (Piece _ King) s [e] = normalMove s e b
doMove b (Piece _ Knight) s [e] = normalMove s e b
doMove b (Piece _ Queen) s [e] = normalMove s e b
doMove b (Piece _ Bishop) s [e] = normalMove s e b
doMove b (Piece _ Rook) s [e] = normalMove s e b

doMove b _ _ _ = b


