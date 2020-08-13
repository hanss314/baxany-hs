module Hooks where

import Piece
import Pos
import Board

-- capturee capturee capturer
preCapture :: Piece -> Pos -> Pos -> Board -> Board
preCapture (Piece c (Chameleon typ)) m n b = preCapture (Piece c typ) m n b
preCapture _ _ _ b = b


postCapture :: Piece -> Pos -> Board -> Board
-- Test for countercapturers
--postCapture (Piece _ (Pawn _)) p b = rawSetPiece p Empty b
postCapture (Piece _ Ghoul) p b = rawSetPiece p Empty b
postCapture (Piece c (Chameleon t)) p b = postCapture (Piece c t) p b
postCapture _ _ b = b

-- moved hook-owner board
preMove :: Piece -> Pos -> Piece -> Pos -> Board -> Board
preMove _ _ _ _ b = b

simpType :: PieceType -> PieceType
simpType (Pawn _) = (Pawn Cham)
simpType (Ace _) = (Ace -10)
simpType (Chameleon t) = simpType t
simpType x = x

postMove :: Piece -> Pos -> Piece -> Pos -> Board -> Board
postMove (Piece mc typ) _ (Piece hc (Chameleon _)) pos b = if mc == hc then b else 
    rawSetPiece pos (Piece hc (Chameleon $ simpType typ)) b

postMove _ _ _ _ b = b

