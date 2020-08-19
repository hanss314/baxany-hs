module Interactions where

import Piece
import Board
import Pos

import Data.List


-- capturer capturee

canCapture :: Piece -> Piece -> Bool
canCapture (Piece c _) (Piece b _) = c /= b
canCapture (Piece _ (Pawn _)) Empty = False
canCapture _ Empty = True
canCapture _ _ = False
