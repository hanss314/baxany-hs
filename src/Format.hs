module Format where

import Board
import Move
import MoveHelper
import Piece
import Json

import Text.JSON
import qualified Data.Vector as V
import Data.Either
import Data.Char
import System.IO


letters = "abcdefghijklmnopqrstuvwxyz"
upper = map toUpper


posToString :: (Int,Int) -> String
posToString (x,y) = letters !! x : show (y+1)

moveToString :: Move -> String
moveToString (N x) = posToString x
moveToString (Chain xs) = xs >>= posToString
moveToString (CharMove s e) = posToString s ++ "&" ++ posToString e
moveToString (Push x) = posToString x
moveToString (Throw s e) = posToString s ++ "@" ++ posToString e
moveToString (PawnMove x) = posToString x

pairToString :: Board -> ((Int, Int), Move) -> (Board, String)
pairToString b (pos, move) = (board, if isRight moved then str else "") where
    moved = doMoveAt b pos move
    board = fromRight b $ moved
    toType (Piece _ t) = t
    piece = filter (/=' ') $ upper $ show $ toType $ getPiece b pos
    isPiece (Piece _ _) = True
    isPiece _ = False
    pieceCount = length . filter isPiece . V.toList . toVector
    cap = if pieceCount board < pieceCount b then "x" else " "
    str = piece ++ (posToString pos) ++ cap ++ (moveToString move)
    

movesToStrings :: Int -> Board -> [((Int, Int), Move)] -> [String]
movesToStrings n board (w:b:xs) = str : movesToStrings (n+1) bboard xs where
    (wboard, white) = pairToString board w
    (bboard, black) = pairToString wboard b
    str = show n ++ ". " ++ white ++ " " ++ black 

movesToStrings n b [w] = [show n ++ ". " ++ (snd $ pairToString b w)]
movesToStrings _ _ [] = []

fromOk (Ok x) = x

main = do
    handle <- openFile "moves.json" ReadMode
    contents <- hGetContents handle
    let moves = decode contents :: Result [((Int, Int), Move)]
    writeFile "moves.txt" $ unlines $ movesToStrings 1 brokeBaxany $ reverse $ fromOk moves
