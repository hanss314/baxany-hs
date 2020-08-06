import Pos
import Piece
import Board
import Transforms

type Move = [Pos]

mb :: Color -> Move -> Move
mb White = id
mb Black = map (\(x,y)->(x,-y)) 

mmb :: Color -> [Move] -> [Move]
mmb c = map (mb c)

-- capturer capturee
canCapture :: Piece -> Piece -> Bool
canCapture (Piece c _) (Piece b _) = c /= b
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

getMoves :: Board -> Piece -> Pos -> [Move]
-- Pawn
getMoves b pawn@(Piece c (Pawn m)) p = forward ++ captures ++ eps where
    basef = [(0,1)]:if m == Start then [[(0,2)]] else []
    forward = filter ((==) Empty . getPiece b . head) $ map (mb c . (p >+)) basef
    captures = filter (canCapture pawn . getPiece b . head) $ map ((p >+) . mb c) [[(1,1)], [(-1,1)]]
    eps = filter (not . null . getEps c b . (p |+) . head . mb c) [[(1,1)], [(-1,1)]]
    

-- default piece has no moves (EnPassant)
getMoves _ _ _ = []

capturingMove :: Pos -> Pos -> Board -> Board
capturingMove s e b = (postCapture p e . rawMovePiece s e . preCapture p e s) b where
    p = getPiece b e

nextPawn :: Piece -> [Pos] -> Piece
nextPawn (Piece c (Pawn Start)) eps = (Piece c (Pawn (EnPassant eps)))
nextPawn p _ = p

-- Assume Move is legal here
doMove :: Board -> Piece -> Pos -> Move -> Board
doMove b pawn@(Piece c (Pawn _)) s@(x,y) m@[e@(mx,my)]
    | x /= mx = capturingMove s e b
    | length epsPos > 0 = (postCapture epCapPie epCap . normalmove . preCapture epCapPie epCap s) b
    | otherwise = normalmove b
    where
        epsPos = getEps c b e
        epCap = head epsPos
        epCapPie = getPiece b epCap
        myeps = [(x,j) | j <- [(min y my)+1..(max y my)-1]]
        normalmove = rawSetPieces $ (e,nextPawn pawn myeps) : (s,Empty) :  map (flip (,) Empty) epsPos


doMove b _ _ _ = b
