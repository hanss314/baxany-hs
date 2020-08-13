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
import qualified Data.Bifunctor as B

isCharMove :: Move -> Bool
isCharMove (CharMove _ _) = True
isCharMove _ = False

capturableSquares :: Board -> Piece -> Pos -> [Pos]
capturableSquares board piece@(Piece c _) pos = (map head . group . sort) $ 
    (concat . map toList . rawGetMoves board piece) pos ++ chariotMoves where
    chariotMoves = (filter ((== (Piece c Chariot)) . getPiece board) $ map (pos|+) uo)
               >>= (\x -> rawGetMoves board (Piece c Chariot) x & filter isCharMove & map (((pos |- x) |+) . mhead))

capturableSquares _ _ _ = []

getEps :: Color -> Board -> Pos -> [Pos]
getEps c board pos@(x,y) = filter (canEp pos . getPiece board) $ map ((,) x) [0..(size board)-1] where
    canEp movepos (Piece pc (Pawn (EnPassant eppos))) = c /= pc && elem movepos eppos
    canEp _ _ = False

rawGetMoves :: Board -> Piece -> Pos -> [Move]
-- Pawn
rawGetMoves b pawn@(Piece c (Pawn m)) p = map N $ forward ++ second ++ captures  ++ eps where
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
rawGetMoves b pie@(Piece _ Joker) p = doubleMoverN b pie p (r4 (1,2) >>= mh)
rawGetMoves b pie@(Piece c Lion) p  = (map mhead $ rawGetMoves b (Piece c King) p) 
                                  >>= (\x -> map (\y->Chain [x, mhead y]) $ rawGetMoves (rawSetPiece p Empty b) (Piece c King) x)

rawGetMoves b pie@(Piece c Dragon) p = (rawGetMoves b (Piece c Rook) p) ++ (rawGetMoves b (Piece c Lion) p)


-- Cannons
rawGetMoves b pie@(Piece _ Pao) p = map N $ uo >>= cannon b pie p
rawGetMoves b pie@(Piece _ Vao) p = map N $ ud >>= cannon b pie p
rawGetMoves b pie@(Piece _ Leo) p = map N $ ua >>= cannon b pie p

-- Knightlikes
rawGetMoves b pie@(Piece c Cobra) p = basicFilter b pie $ p >+ r4 (2,2)
rawGetMoves b pie@(Piece c Camel) p = basicFilter b pie $ p >+ (r4 (3,1) >>= mh)
rawGetMoves b pie@(Piece c Zebra) p = basicFilter b pie $ p >+ (r4 (3,2) >>= mh)
rawGetMoves b pie@(Piece c Giraffe) p = basicFilter b pie $ p >+ (r4 (4,2) >>= mh)
rawGetMoves b pie@(Piece c Kangaroo) p = basicFilter b pie $ p >+ (r4 (4,3) >>= mh)

-- Other special ones
rawGetMoves b pie@(Piece c General) p = rawGetMoves b (Piece c King) p ++ swaps
    where
        swaps = map N $ filter ((==) (Piece c King) . getPiece b) $ p >+ [(x,y) | x<-[-2..2], y<-[-2..2]]

rawGetMoves b pie@(Piece c Lance) p = basicFilterSlider b pie p $ [mpb c (0,1)]
rawGetMoves b pie@(Piece c Imitator) p =
    filter (\m -> ((getPiece b $ mhead m) == Empty) 
               || (p `elem` capturableSquares b (getPiece b (mhead m)) (mhead m))) $ 
    filter (\m -> (getType $ getPiece b $ mhead m) /= Imitator) qmoves 
    where
        qmoves = rawGetMoves b (Piece c Queen) p

rawGetMoves b pie@(Piece c Chariot) p = (map N basics) ++ carries ++ mageBoost where
    getCarries :: Pos -> Pos -> Pos -> [Pos]
    getCarries char carr step
        | charNextPiece /= Empty = []
        | carrNextPiece == Empty = charNextPos : getCarries charNextPos carrNextPos step
        | canCapture pie carrNextPiece = [charNextPos]
        | otherwise = []
        where
            charNextPos = char |+ step
            charNextPiece = if carr |- char == step then Empty else getPiece b charNextPos
            carrNextPos = carr |+ step
            carrNextPiece = getPiece b carrNextPos

    basics = ua >>= noCapSlider b pie p 
    carries = (filter (isColor c . getPiece b) $ map (p|+) uo) >>= (\x -> ua >>= (map (CharMove x) . getCarries p x))
    maybeMageBoost = (filter (isColor c . getPiece b . (p|+)) uo) >>=
                        (\d -> map (\k->CharMove (p|+d) k) 
                            $ filter (\k -> canCapture pie (getPiece b k) && (canCapture pie $ getPiece b (k |+ d))) 
                            $ p >+ (r4 (1,2) >>= mh))
    mageBoost = if any (\x -> x`elem`[Piece c Mage, Piece c (Chameleon Mage)]) $ 
                            map (getPiece b . (p|+)) ua then maybeMageBoost else []

rawGetMoves b (Piece _ Elephant) p = map Push $ filter ((/=Block) . getPiece b) $ p >+ ua
rawGetMoves b (Piece c (Chameleon t)) p = rawGetMoves b (Piece c t) p
rawGetMoves b (Piece _ Ghoul) p = map N $ filter (/=p) $ filter ((==Empty) . getPiece b) allBoard where
    allBoard = [(x,y)|x<-[0..(size b)-1], y<-[0..(size b)-1]]

rawGetMoves b (Piece c (Ace _)) p = rawGetMoves b (Piece c King) p

-- default piece has no moves
rawGetMoves _ _ _ = []

getMoves :: Board -> Piece -> Pos -> [Move]
getMoves board piece@(Piece c _) pos = rawMoves ++ mageBoost where
    rawMoves = rawGetMoves board piece pos
    hasMage = any (\x->x`elem`[Piece c Mage, Piece c (Chameleon Mage)]) $ map (getPiece board) $ pos >+ ua
    maybeMageBoost = rawGetMoves board (Piece c Knight) pos
    mageBoost = if hasMage then maybeMageBoost else []
    
getMoves _ _ _ = []

movesAt :: Board -> Pos -> [Move]
movesAt b pos = getMoves b (getPiece b pos) pos

capturingMove :: Pos -> Pos -> Board -> Board
capturingMove s e b = b & preCapture p e s & rawMovePiece s e & postCapture p e where
    p = getPiece b e

doMoveAt :: Board -> Pos -> Move -> Board
doMoveAt b pos move = doMove b (getPiece b pos) pos move

applyList :: [a -> a] -> a -> a
applyList funcs source = foldl' (\x f -> f x) source funcs

doMove :: Board -> Piece -> Pos -> Move -> Board
doMove b pie pos move = if move `elem` getMoves b pie pos then switch $ final else b where
    squares = [(x,y) | x<-[0..(size b)-1], y<-[0..(size b)-1]]
    prefuncs = map (\x -> preMove pie pos (getPiece b x) x) squares
    pre = applyList prefuncs b
    moved = rawDoMove pre pie pos move
    postfuncs = map (\x -> postMove pie pos (getPiece moved x) x) squares
    final = applyList postfuncs moved

-- Assume Move is legal here
rawDoMove :: Board -> Piece -> Pos -> Move -> Board

-- Pawn
rawDoMove b pawn@(Piece c (Pawn _)) s@(x,y) (N e@(mx,my))
    | length epsPos > 0 = b & preCapture epCapPie epCap s & normalmove & postCapture epCapPie epCap
    | otherwise = normalmove b
    where
        epsPos = getEps c b e
        epCap = head epsPos
        epCapPie = getPiece b epCap
        myeps = [(x,j) | j <- [(min y my)+1..(max y my)-1]]
        normalmove = rawSetPieces $ (e,nextPawn pawn myeps) : (s,Empty) :  map (flip (,) Empty) epsPos

rawDoMove b (Piece c Lance) s (N e@(x,y)) = promotion end
    where
        end = normalMove s e b
        target = if c == Black then 0 else (size b) - 1
        promotion = if y == target then rawSetPiece e (Piece c Rook) else id

rawDoMove b pie@(Piece c General) s (N e) = if target /= (Piece c King) then normalMove s e b else
    rawSetPieces [(s, target), (e, pie)] b
    where 
        target = getPiece b e
    
rawDoMove b pie s (Chain [e1, e2]) = 
    if getPiece firstMove e1 == pie then secondMove else firstMove where
    firstMove = normalMove s e1 b
    secondMove = normalMove e1 e2 firstMove

rawDoMove b pie@(Piece c Chariot) s (CharMove carr e) = 
    b & rawSetPiece s Empty & (\x -> rawDoMove x (getPiece b carr) carr (N (e |+ carr |- s))) & rawSetPiece e pie

rawDoMove b (Piece c Ghoul) s m@(N e) = if getPiece b e == Empty then normalMove s e b else
    rawSetPieces [(e, Empty), (s, Empty)] b

rawDoMove b (Piece c (Chameleon t)) s m = rawDoMove b (Piece c t) s m
rawDoMove b (Piece c (Ace i)) s (N e) = normalMove s e updated where
    promotable (Piece _ Empress) = 10
    promotable (Piece _ (Pawn _)) = 0
    promotable (Piece _ _) = 1
    promotable _ = 0
    ni = i + (promotable $ getPiece b e)
    newPiece = if ni < 10 then (Piece c (Ace ni)) else (Piece c Joker)
    updated = rawSetPiece s newPiece b

rawDoMove b pie s (Push e) = (postCapture (snd capture) e . moved . preCapture (snd capture) (fst capture) s) b where
    getPushList :: Pos -> Pos -> [(Pos, Piece)]
    getPushList start step = case getPiece b $ step |+ start of
            Empty -> [(start |+ step, getPiece b start), (start |+ step, Empty)]
            Block -> [(start, getPiece b start)]
            _ -> (start |+ step, getPiece b start) : getPushList (start |+ step) step

    delta = e |- s
    pushList = (s, Empty) :  getPushList s delta
    moved = rawSetPieces (init pushList)
    capture = B.first (|- delta) $ last pushList


rawDoMove b _ s (N e) = normalMove s e b
rawDoMove b _ _ _ = b

