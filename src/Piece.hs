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
pieceToInt Null           = -1
pieceToInt (Pawn _)       = 30
pieceToInt Knight         = 24
pieceToInt Bishop         = 18
pieceToInt Rook           = 12
pieceToInt Queen          = 06
pieceToInt King           = 00
pieceToInt Amazon         = 19
pieceToInt Chancellor     = 14
pieceToInt Cardinal       = 08
pieceToInt HookMover      = 07
pieceToInt Lance          = 01
pieceToInt Knightmare     = 25
pieceToInt Empress        = 17
pieceToInt General        = 31
pieceToInt Pao            = 02
pieceToInt Vao            = 26
pieceToInt Leo            = 32
pieceToInt Mage           = 03
pieceToInt Cobra          = 27
pieceToInt Camel          = 21
pieceToInt Zebra          = 15
pieceToInt Giraffe        = 09
pieceToInt Kangaroo       = 04
pieceToInt Lion           = 10
pieceToInt Dragon         = 20
pieceToInt Imitator       = 33
pieceToInt Chariot        = 16
pieceToInt Elephant       = 22
pieceToInt (Chameleon _)  = 28
pieceToInt (Ace _)        = 34
pieceToInt Joker          = 13
pieceToInt Jack           = 05
pieceToInt Trebuchet      = 11
pieceToInt Ghoul          = 23
pieceToInt Gryphon        = 29

intToPiece :: Int -> PieceType
intToPiece (-1) = Null           
intToPiece 30 = (Pawn Start)       
intToPiece 24 = Knight         
intToPiece 18 = Bishop         
intToPiece 12 = Rook           
intToPiece 06 = Queen          
intToPiece 00 = King           
intToPiece 19 = Amazon         
intToPiece 14 = Chancellor     
intToPiece 08 = Cardinal       
intToPiece 07 = HookMover      
intToPiece 01 = Lance 
intToPiece 25 = Knightmare     
intToPiece 17 = Empress        
intToPiece 31 = General        
intToPiece 02 = Pao            
intToPiece 26 = Vao            
intToPiece 32 = Leo            
intToPiece 03 = Mage           
intToPiece 27 = Cobra          
intToPiece 21 = Camel          
intToPiece 15 = Zebra          
intToPiece 09 = Giraffe        
intToPiece 04 = Kangaroo       
intToPiece 10 = Lion           
intToPiece 20 = Dragon         
intToPiece 33 = Imitator       
intToPiece 16 = Chariot        
intToPiece 22 = Elephant       
intToPiece 28 = (Chameleon Null)  
intToPiece 34 = (Ace 0)        
intToPiece 13 = Joker          
intToPiece 05 = Jack           
intToPiece 11 = Trebuchet      
intToPiece 23 = Ghoul          
intToPiece 29 = Gryphon        
