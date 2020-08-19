module Piece where

import Pos (Pos)

data Color = Black | White
   deriving (Eq)

data PawnState = Start | EnPassant [Pos] | Normal | Cham deriving Eq

other :: Color -> Color
other White = Black
other Black = White

instance Show Color where
    show Black = "b"
    show White = "w"

data PieceType =
             Null
           | Pawn PawnState
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
    show Null           = "  "
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

isColor :: Color -> Piece -> Bool
isColor c (Piece pc _) = c == pc
isColor c _ = False

oc = other . color

getType (Piece _ t) = t
getType _ = Null

instance Show Piece where
    show (Piece c t) = (show c) ++ (show t)
    show Empty = "   "
    show Block = "XXX"

pieceToInt :: PieceType -> Int
pieceToInt Null           = 00
pieceToInt (Pawn _)       = 01
pieceToInt Knight         = 02
pieceToInt Bishop         = 03
pieceToInt Rook           = 04
pieceToInt Queen          = 05
pieceToInt King           = 06
pieceToInt Amazon         = 07
pieceToInt Chancellor     = 08
pieceToInt Cardinal       = 09
pieceToInt HookMover      = 10
pieceToInt Lance          = 11
pieceToInt Knightmare     = 12
pieceToInt Empress        = 13
pieceToInt General        = 14
pieceToInt Pao            = 15
pieceToInt Vao            = 16
pieceToInt Leo            = 17
pieceToInt Mage           = 18
pieceToInt Cobra          = 19
pieceToInt Camel          = 20
pieceToInt Zebra          = 21
pieceToInt Giraffe        = 22
pieceToInt Kangaroo       = 23
pieceToInt Lion           = 24
pieceToInt Dragon         = 25
pieceToInt Imitator       = 26
pieceToInt Chariot        = 27
pieceToInt Elephant       = 28
pieceToInt (Chameleon _)  = 29
pieceToInt (Ace _)        = 30
pieceToInt Joker          = 31
pieceToInt Jack           = 32
pieceToInt Trebuchet      = 33
pieceToInt Ghoul          = 34
pieceToInt Gryphon        = 35

intToPiece :: Int -> PieceType
intToPiece 00 = Null
intToPiece 01 = (Pawn Start)
intToPiece 02 = Knight
intToPiece 03 = Bishop
intToPiece 04 = Rook
intToPiece 05 = Queen
intToPiece 06 = King
intToPiece 07 = Amazon
intToPiece 08 = Chancellor
intToPiece 09 = Cardinal
intToPiece 10 = HookMover
intToPiece 11 = Lance
intToPiece 12 = Knightmare
intToPiece 13 = Empress
intToPiece 14 = General
intToPiece 15 = Pao
intToPiece 16 = Vao
intToPiece 17 = Leo
intToPiece 18 = Mage
intToPiece 19 = Cobra
intToPiece 20 = Camel
intToPiece 21 = Zebra
intToPiece 22 = Giraffe
intToPiece 23 = Kangaroo
intToPiece 24 = Lion
intToPiece 25 = Dragon
intToPiece 26 = Imitator
intToPiece 27 = Chariot
intToPiece 28 = Elephant
intToPiece 29 = (Chameleon Null)
intToPiece 30 = (Ace 0)
intToPiece 31 = Joker
intToPiece 32 = Jack
intToPiece 33 = Trebuchet
intToPiece 34 = Ghoul
intToPiece 35 = Gryphon
