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

canCapture :: Piece -> Piece -> Bool
canCapture (Piece c _) (Piece b _) = c /= b
canCapture _ _ = False

-- capturee capturee capturer
preCapture :: Piece -> Pos -> Pos -> Board -> Board
preCapture ep@(Piece _ (EnPassant p)) e s b = preCapture (getPiece b p) e s b
preCapture _ _ _ b = b

postCapture :: Piece -> Pos -> Board -> Board
postCapture ep@(Piece _ (EnPassant p)) e b = (postCapture target e . rawSetPiece p Empty) b where
    target = getPiece b p
postCapture _ _ b = b

getMoves :: Board -> Piece -> Pos -> [Move]
-- Pawn
getMoves b pawn@(Piece c (Pawn m)) p = forward ++ captures where
    basef = [(0,1)]:if m then [[(0,2)]] else []
    forward = filter ((==) Empty . getPiece b . head) $ map (mb c . (p >+)) basef
    captures = filter (canCapture pawn . getPiece b . head) $ map (mb c . (p >+)) [[(1,1)], [(-1,1)]]

-- default piece has no moves (EnPassant)
getMoves _ _ _ = []

capturingMove :: Pos -> Pos -> Board -> Board
capturingMove s e b = (postCapture p e . rawMovePiece s e . preCapture p e s) b where
    p = getPiece b e

-- Assume Move is legal here
doMove :: Board -> Piece -> Pos -> Move -> Board
doMove b pawn@(Piece c (Pawn _)) s@(x,y) m@[e@(mx,my)]
    | x /= mx = capturingMove s e b
    | otherwise = (rawSetPieces eps . rawMovePiece s e) b where
        eps = [((x,j), Piece c (EnPassant e)) | j <- [(min y my)+1..(max y my)-1]]

doMove b _ _ _ = b
