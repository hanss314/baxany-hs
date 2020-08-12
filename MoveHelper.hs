module MoveHelper where

import Board
import Piece
import Pos
import Hooks
import Interactions

import Data.Function
import Data.List

data Move = N [Pos] | MageMove Pos | CharMove Pos Pos deriving (Show, Eq)

mmap :: (Pos -> Pos) -> Move -> Move
mmap f (N xs) = N $ map f xs
mmap f (MageMove x) = MageMove $ f x 
mmap f (CharMove char to) = CharMove char $ f to

mfilter :: (Pos -> Bool) -> Move -> Move
mfilter f (N xs) = N $ filter f xs
mfilter f (MageMove x) = if f x then MageMove x else N []
mfilter f m@(CharMove char x) = if f x then m else N []

mpb :: Color -> Pos -> Pos
mpb White = id
mpb Black = (\(x,y)->(x,-y))

mb :: Color -> Move -> Move
mb White = id
mb Black = mmap (\(x,y)->(x,-y)) 

mmb :: Color -> [Move] -> [Move]
mmb c = map (mb c)

mhead :: Move -> Pos
mhead (MageMove x) = x
mhead (N (x:_)) = x
mhead (CharMove _ x) = x

mageify :: Move -> Move
mageify = MageMove . mhead

listify :: Pos -> Move
listify x = N [x]

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

noCapSlider :: Board -> Piece -> Pos -> Pos -> [Pos]
noCapSlider board piece start step
    | nextPiece == Empty = nextPos : noCapSlider board piece nextPos step
    | otherwise = []
    where
        nextPos = start |+ step
        nextPiece = getPiece board nextPos

firstCap :: Board -> Piece -> Pos -> Pos -> [Pos]
firstCap board piece start step
    | nextPiece == Empty = firstCap board piece nextPos step
    | canCapture piece nextPiece = [nextPos]
    | otherwise = []
    where
        nextPos = start |+ step
        nextPiece = getPiece board nextPos
        

basicFilterSlider :: Board -> Piece -> Pos -> [Pos] -> [Move]
basicFilterSlider board piece pos steps = map listify $ steps >>= basicSlider board piece pos

normalMove :: Pos -> Pos -> Board -> Board
normalMove start end b = case getPiece b end of
    Block -> b
    Empty -> rawMovePiece start end b
    x -> b & preCapture x end start & rawMovePiece start end & postCapture x end

nextPawn :: Piece -> [Pos] -> Piece
nextPawn (Piece c (Pawn Start)) eps = (Piece c (Pawn (EnPassant eps)))
nextPawn p _ = p

doubleMoverN :: Board -> Piece -> Pos -> [Pos] -> [Move]
doubleMoverN board piece start deltas = (map listify . map head . group . sort) moves
    where
        firstMoves = start : (deltas >>= noCapSlider board piece start)
        secondMoves = firstMoves >>= (\x -> concat $ map (basicSlider board piece x) deltas)
        moves = filter (/=start) $ firstMoves ++ secondMoves

cannon :: Board -> Piece -> Pos -> Pos -> [Pos]
cannon board piece start step
    | nextPiece == Empty = nextPos : cannon board piece nextPos step
    | otherwise =  firstCap board piece nextPos step
    where
        nextPos = start |+ step
        nextPiece = getPiece board nextPos

toList :: Move -> [Pos]
toList (N xs) = xs
toList (MageMove x) = [x]

