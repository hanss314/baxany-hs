{-# LANGUAGE BangPatterns #-}

module Board where

import qualified Data.Vector as V
import Data.List
import qualified Data.Bifunctor as B
import Pos (Pos)
import Piece
import Data.Function 

data Board = Board {
     toVector :: !(V.Vector Piece)
     -- | Player making the next move.
   , turn :: !Color
   , size :: !Int
   } deriving (Eq)

switch :: Board -> Board
switch !board = board { 
    turn = other $ turn board
,   toVector = V.map (clearEP . other $ turn board) $ toVector board
}

clearEP :: Color -> Piece -> Piece
clearEP tc p@(Piece pc (Pawn (EnPassant _))) = if tc == pc then (Piece pc (Pawn Normal)) else p
clearEP _ x = x

emptyBoard :: Int -> Board
emptyBoard n = Board 
    {
        toVector = V.replicate (n*n) Empty
    ,   turn = White
    ,   size = n
    }

ctoi :: Board -> Pos -> Int
ctoi b (x,y) = y*(size b) + x

inRange :: Board -> Pos -> Bool
inRange b (x,y) = 0 <= x && x < (size b) && 0 <= y && y < (size b)

getPiece :: Board -> Pos -> Piece
getPiece b p = if inRange b p then (toVector b) V.! (ctoi b p) else Block
(!) = getPiece

rawSetPiece :: Pos -> Piece -> Board -> Board
rawSetPiece pos pie b = if not $ inRange b pos then b else
    b { toVector = toVector b V.// [(ctoi b pos, pie)] }

rawSetPieces :: [(Pos, Piece)] -> Board -> Board
rawSetPieces p b = b {toVector = toVector b V.// (map (B.first (ctoi b)) $ filter (inRange b . fst) p)}

rawMovePiece :: Pos -> Pos -> Board -> Board
rawMovePiece s d b = b { toVector = toVector b V.// [(ctoi b d, b!s), (ctoi b s, Empty)] }

hex = "0123456789abcdefghijklmnopqrstuvwxyz"

instance Show Board where
    show board = (unlines . reverse . zipWith showLine [0..] . splitRows . toVector) board
        ++ "\n  " ++ (intercalate " " $ map showFile [0..(size board)-1]) ++ "\nTurn: " ++ (show $ turn board)
      where
        showLine :: Int -> V.Vector Piece -> String
        splitRows :: V.Vector a -> [V.Vector a]
        n = size board
        showLine rank v = (hex !! rank) : ' ' : (intercalate " " $ map show $ V.toList v)
        showFile f = ' ' : (hex !! f) : " "
        splitRows v = if V.length v <= n then [v] else V.take n v : (splitRows $ V.drop n v)

regularChess :: Board
regularChess = rawSetPieces [((x,y), Piece (if y==1 then White else Black) (Pawn Start)) | x<-[0..7], y<-[1,6]] $ emptyBoard 8

blackback = [
    Lance, Lion, Jack, Ace 0, Chariot, Knightmare, Amazon, Empress, 
    King, Amazon, Knightmare, Chariot, Ace 0, Jack, Lion, Lance]

whiteback = reverse blackback

secondrow = [
    Chancellor, Dragon, Chameleon Null, Elephant, Trebuchet, Leo, Queen, HookMover] & (\x -> x ++ reverse x)

thirdrow = [
    Rook, Ghoul, Imitator, Kangaroo, Giraffe, General, Vao, Camel] & (\x -> x ++ reverse x)

fourthrow = [
    Pao, Gryphon, Zebra, Camel, Cobra, Knight, Mage, Bishop] & (\x -> x ++ reverse x)

pawns = take 16 $ repeat (Pawn Start)

centre = take (6*16) $ repeat Empty

whites = map (Piece White) $ whiteback ++ secondrow ++ thirdrow ++ fourthrow ++ pawns
blacks = map (Piece Black) $ pawns ++ fourthrow ++ thirdrow ++ secondrow ++ blackback

baxanyList = whites ++ centre ++ blacks

baxany = Board {
    toVector = V.fromList baxanyList,
    turn = White,
    size = 16
}
