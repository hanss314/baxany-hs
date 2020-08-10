module Hooks where

import Piece
import Pos
import Board

-- capturee capturee capturer
preCapture :: Piece -> Pos -> Pos -> Board -> Board
preCapture _ _ _ b = b

postCapture :: Piece -> Pos -> Board -> Board
postCapture _ _ b = b
