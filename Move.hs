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
preCapture _ _ _ = id

postCapture :: Piece -> Pos -> Board -> Board
postCapture _ _ = id

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

doMove :: Board -> Piece -> Pos -> Move -> Maybe Board
doMove b pawn@(Piece c (Pawn _)) s@(x,y) m@[e@(mx,my)]
    | not $ elem m (getMoves b pawn s) = Nothing
    | x /= mx = Just $ capturingMove s e b
    | otherwise = Just $ (rawSetPieces eps . rawMovePiece s e) b where
        eps = [((x,j), Piece c (EnPassant e)) | j <- [(min y my)+1..(max y my)-1]]

doMove _ _ _ _ = Nothing
