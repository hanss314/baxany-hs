module MoveHelper where

import Board
import Piece
import Pos
import Hooks
import Interactions

import Data.Function

type Move = [Pos]

mb :: Color -> Move -> Move
mb White = id
mb Black = map (\(x,y)->(x,-y)) 

mmb :: Color -> [Move] -> [Move]
mmb c = map (mb c)

listify :: a -> [a]
listify x = [x]

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

normalMove :: Pos -> Pos -> Board -> Board
normalMove start end b = case getPiece b end of
    Block -> b
    Empty -> rawMovePiece start end b
    x -> b & preCapture x end start & rawMovePiece start end & postCapture x end

nextPawn :: Piece -> [Pos] -> Piece
nextPawn (Piece c (Pawn Start)) eps = (Piece c (Pawn (EnPassant eps)))
nextPawn p _ = p

