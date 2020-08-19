module MoveHelper where

import Board
import Piece
import Pos
import Hooks
import Interactions

import Data.Function
import Data.List

data Move = N Pos | PawnMove Pos | CharMove Pos Pos | Chain [Pos] | Push Pos | Throw Pos Pos deriving (Show, Eq)


mpb :: Color -> Pos -> Pos
mpb White = id
mpb Black = (\(x,y)->(x,-y))

mhead :: Move -> Pos
mhead (N x) = x
mhead (Chain xs) = last xs
mhead (CharMove _ x) = x
mhead (Push x) = x
mhead (Throw _ x) = x
mhead (PawnMove x) = x

toList :: Move -> [Pos]
toList (N x) = [x]
toList (Chain xs) = xs
toList (CharMove _ x) = []
toList (Throw _ x) = [x]
toList (Push x) = [x]
toList (PawnMove x) = [x]

basicFilter :: Board -> Piece -> [Pos] -> [Move]
basicFilter b p = (map N . filter (canCapture p . getPiece b))

limitedSlider :: Int -> Board -> Pos -> Pos -> [Pos]
limitedSlider n b s d = take n $ noCapSlider b s d

basicSlider :: Board -> Piece -> Pos -> Pos -> [Pos]
basicSlider board piece start step
    | nextPiece == Empty = nextPos : basicSlider board piece nextPos step
    | canCapture piece nextPiece = [nextPos]
    | otherwise = []
    where 
        nextPos = start |+ step
        nextPiece = getPiece board nextPos

noCapSlider :: Board -> Pos -> Pos -> [Pos]
noCapSlider board start step
    | nextPiece == Empty = nextPos : noCapSlider board nextPos step
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

doubleMoverN :: Board -> Piece -> Pos -> [Pos] -> [Move]
doubleMoverN board piece start deltas = (map (N . head) . group . sort) moves
    where
        firstMoves = start : (deltas >>= noCapSlider board start)
        secondMoves = firstMoves >>= (\x -> concat $ map (basicSlider board piece x) deltas)
        capMoves = filter (not . flip elem firstMoves) $ deltas >>= basicSlider board piece start
        moves = filter (/=start) $ firstMoves ++ secondMoves ++ capMoves

cannon :: Board -> Piece -> Pos -> Pos -> [Pos]
cannon board piece start step
    | nextPiece == Empty = nextPos : cannon board piece nextPos step
    | otherwise =  firstCap board piece nextPos step
    where
        nextPos = start |+ step
        nextPiece = getPiece board nextPos


