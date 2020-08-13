module MoveHelper where

import Board
import Piece
import Pos
import Hooks
import Interactions

import Data.Function
import Data.List

data Move = N Pos | CharMove Pos Pos | Chain [Pos] | Push Pos deriving (Show, Eq)

mmap :: (Pos -> Pos) -> Move -> Move
mmap f (Chain xs) = Chain $ map f xs
mmap f (N x) = N $ f x 
mmap f (CharMove char to) = CharMove char $ f to
mmap f (Push x) = Push $ f x


mpb :: Color -> Pos -> Pos
mpb White = id
mpb Black = (\(x,y)->(x,-y))

mb :: Color -> Move -> Move
mb White = id
mb Black = mmap (\(x,y)->(x,-y)) 

mmb :: Color -> [Move] -> [Move]
mmb c = map (mb c)

mhead :: Move -> Pos
mhead (N x) = x
mhead (Chain xs) = last xs
mhead (CharMove _ x) = x
mhead (Push x) = x

toList :: Move -> [Pos]
toList (N x) = [x]
toList (Chain xs) = xs
toList (CharMove _ x) = [x]
toList (Push x) = [x]

basicFilter :: Board -> Piece -> [Pos] -> [Move]
basicFilter b p = (map N . filter (canCapture p . getPiece b))

limitedSlider :: Int -> Board -> Piece -> Pos -> Pos -> [Pos]
limitedSlider n b p s d = take n $ noCapSlider b p s d

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
basicFilterSlider board piece pos steps = map N $ steps >>= basicSlider board piece pos

normalMove :: Pos -> Pos -> Board -> Board
normalMove start end b = case getPiece b end of
    Block -> b
    Empty -> rawMovePiece start end b
    x -> b & preCapture x end start & rawMovePiece start end & postCapture x end

nextPawn :: Piece -> [Pos] -> Piece
nextPawn (Piece c (Pawn Start)) eps = (Piece c (Pawn (EnPassant eps)))
nextPawn p _ = p

doubleMoverN :: Board -> Piece -> Pos -> [Pos] -> [Move]
doubleMoverN board piece start deltas = (map (N . head) . group . sort) moves
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


