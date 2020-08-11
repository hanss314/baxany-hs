module Hooks where

import Piece
import Pos
import Board

-- capturee capturee capturer
preCapture :: Piece -> Pos -> Pos -> Board -> Board
preCapture _ _ _ b = b

postCapture :: Piece -> Pos -> Board -> Board
-- Test for countercapturers
--postCapture (Piece _ (Pawn _)) p b = rawSetPiece p Empty b
postCapture (Piece _ Ghoul) p b = rawSetPiece p Empty b
postCapture _ _ b = b
