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
           | Amazon | Chancellor | Cardinal
           | HookMover
           | Lance
           | Knightmare
           | Empress
           | General
           | Pao | Vao | Leo
           | Mage
           | Cobra | Camel | Zebra | Giraffe | Kangaroo
           | Lion
           | Dragon
           | Imitator
           | Chariot
           | Elephant
           | Chameleon PieceType
           | Ace Int | Joker | Jack
           | Trebuchet
           | Ghoul
           | Gryphon
   deriving (Eq)

instance Show PieceType where
    show (Pawn _)       = "P "
    show Knight         = "N "
    show Bishop         = "B "
    show Rook           = "R "
    show Queen          = "Q "
    show King           = "K "
    show Amazon         = "A " 
    show Chancellor     = "Ch"
    show Cardinal       = "Cn"
    show HookMover      = "H "
    show Lance          = "La"
    show Knightmare     = "Nm"
    show Empress        = "Em"
    show General        = "G "
    show Pao            = "Pa"
    show Vao            = "Va"
    show Leo            = "L "
    show Mage           = "M "
    show Cobra          = "Co"
    show Camel          = "Ca"
    show Zebra          = "Z "
    show Giraffe        = "Gi"
    show Kangaroo       = "Kg"
    show Lion           = "Li"
    show Dragon         = "D "
    show Imitator       = "I "
    show Chariot        = "Cr"
    show Elephant       = "El"
    show (Chameleon _)  = "Cm"
    show (Ace _)        = "Ac"
    show Joker          = "J "
    show Jack           = "Ja"
    show Trebuchet      = "Tr"
    show Ghoul          = "Gh"
    show Gryphon        = "Gy"

data Piece =  Piece !Color !PieceType | Empty | Block
   deriving Eq

color :: Piece -> Color
color (Piece c _) = c
oc = other . color

instance Show Piece where
    show (Piece c t) = (show c) ++ (show t)
    show Empty = "   "
    show Block = "XXX"
