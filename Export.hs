import Move
import Board
import Piece
import Pos
import MoveHelper

import Asterius.Aeson
import Asterius.ByteString
import Asterius.Text
import Asterius.Types
import Foreign.StablePtr

regularJSBoard :: IO (StablePtr Board)
regularJSBoard = newStablePtr regularChess

foreign export javascript "regularBoard" regularJSBoard :: IO (StablePtr Board)

main = return ()
