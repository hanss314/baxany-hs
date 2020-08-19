import Text.JSON
import Text.JSON.Types
import qualified Data.Vector as V

import Board
import Piece
import MoveHelper

l2o = JSObject . toJSObject

jsPiece = 1 :: Int
jsEmpty = 0 :: Int
jsBlock = 2 :: Int

get_result :: JSObject a -> String -> Result a
get_result obj str = maybe (Error$ "Field " <> str <> " not found") Ok $ get_field obj str

instance JSON Board where
    showJSON board = l2o [
        ("size", showJSON $ size board),
        ("turn", showJSON $ turn board),
        ("board", showJSON $ map showJSON $ V.toList $ toVector board)]

    readJSON (JSObject obj) = do
        size <- get_result obj "size" >>= readJSON
        turn <- get_result obj "turn" >>= readJSON
        board <- get_result obj "board" >>= readJSON
        return Board { size = size, turn = turn, toVector = V.fromList board}
    
    readJSON _ = Error "Board must be object"

    

instance JSON Color where
    showJSON Black = JSBool True
    showJSON White = JSBool False
    readJSON (JSBool True) = Ok Black
    readJSON (JSBool False) = Ok White
    readJSON _ = Error "Color must be bool"

instance JSON Piece where    
    showJSON (Piece c t) = l2o [("type", showJSON jsPiece), ("color", showJSON c), ("piece", showJSON t)]
    showJSON Empty = l2o [("type", showJSON jsEmpty)]
    showJSON Block = l2o [("type", showJSON jsBlock)]                
    
    readJSON (JSObject obj) = do
        t <- get_result obj "type" >>= readJSON
        if t == jsEmpty then return Empty
        else if t == jsBlock then return Block
        else do
            c <- get_result obj "color" >>= readJSON
            p <- get_result obj "piece" >>= readJSON
            return (Piece c p)

    readJSON _ = Error "Piece must be object"
   
instance JSON PieceType where
    showJSON p@(Pawn s)       = l2o [("type", showJSON $ pieceToInt p), ("state", showJSON s)]
    showJSON p@(Chameleon t)  = l2o [("type", showJSON $ pieceToInt p), ("piece", showJSON t)]
    showJSON p@(Ace caps)     = l2o [("type", showJSON $ pieceToInt p), ("caps", showJSON caps)]
    showJSON p = l2o [("type", showJSON $ pieceToInt p)]
    readJSON (JSObject obj) = do
        t <- get_result obj "type" >>= readJSON
        let p = intToPiece t
        case p of 
            (Pawn _) -> get_result obj "state" >>= readJSON >>= (return . Pawn)
            (Ace _)  -> get_result obj "caps" >>= readJSON >>= (return . Ace)
            (Chameleon _) -> get_result obj "piece" >>= readJSON >>= (return . Chameleon)
            _ -> return p

    readJSON _ = Error "PieceType must be object"

instance JSON PawnState where
    showJSON Start          = l2o [("type", showJSON (0 :: Int))]
    showJSON (EnPassant xs) = l2o [("type", showJSON (1 :: Int)), ("pos", showJSON $ map showJSON xs)]
    showJSON Normal         = l2o [("type", showJSON (2 :: Int))]
    showJSON Cham           = l2o [("type", showJSON (3 :: Int))]

    readJSON (JSObject obj) = do
        t <- (get_result obj "type" >>= readJSON) :: Result Int
        case t of
            0 -> return Start
            2 -> return Normal
            3 -> return Cham
            1 -> get_result obj "pos" >>= readJSON >>= mapM readJSON >>= (return . EnPassant)
            _ -> Error "Invalid pawnstate"

    readJSON _ = Error "PawnState must be object"
