module Piece where

import Pos (Pos)

data Color = Black | White
   deriving (Eq)

data PawnState = Start | EnPassant [Pos] | Normal deriving Eq

other :: Color -> Color
other White = Black
other Black = White

instance Show Color where
    show Black = "b"
    show White = "w"

data PieceType =
             Pawn PawnState
           | Knight
           | Bishop
           | Rook
           | Queen
           | King
   deriving (Eq)

instance Show PieceType where
   show (Pawn _)  = "P "
   show Knight = "N "
   show Bishop = "B "
   show Rook   = "R "
   show Queen  = "Q "
   show King   = "K "

data Piece =  Piece !Color !PieceType | Empty | Block
   deriving Eq

color :: Piece -> Color
color (Piece c _) = c
oc = other . color

instance Show Piece where
    show (Piece c t) = (show c) ++ (show t)
    show Empty = "   "
    show Block = "XXX"
