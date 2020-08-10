module Interactions where

import Piece

-- capturer capturee
canCapture :: Piece -> Piece -> Bool
canCapture (Piece c _) (Piece b _) = c /= b
canCapture _ Empty = True
canCapture _ _ = False
