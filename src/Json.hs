{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Json where

import Text.JSON
import Text.JSON.Types
import qualified Data.Vector as V

import Data.Scientific
import qualified Data.Aeson.Types as A
import Data.Bifunctor
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Data.Tuple (swap)
import Data.List (elemIndex)

import Board
import Piece
import MoveHelper
import Pos

l2o = JSObject . toJSObject
ptoi (x,y) = 16*y+x
itop = swap . flip divMod 16

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

safeFromRational x = case fromRationalRepetend (Just 1) x of
    Left  (y, _) -> y
    Right (y, _) -> y

textToAeson :: JSValue -> A.Value
textToAeson JSNull = A.Null
textToAeson (JSBool b) = A.Bool b
textToAeson (JSRational b r) = A.Number $ fromIntegral $ truncate r
textToAeson (JSString s) = A.String $ T.pack $ fromJSString s
textToAeson (JSArray vals) = A.Array $ V.fromList $ map textToAeson vals
textToAeson (JSObject obj) = A.Object $ H.fromList $ map (bimap T.pack textToAeson) $ fromJSObject obj

aesonToText :: A.Value -> JSValue
aesonToText A.Null = JSNull
aesonToText (A.Bool b) = JSBool b
aesonToText (A.Number s) = JSRational False $ fromIntegral $ truncate s
aesonToText (A.String s) = JSString $ toJSString $ T.unpack s
aesonToText (A.Array vals) = JSArray $ map aesonToText $ V.toList vals
aesonToText (A.Object obj) = JSObject $ toJSObject $ map (bimap T.unpack aesonToText) $ H.toList obj

jsonToAeson :: (JSON a) => a -> A.Value
aesonToJson :: (JSON a) => A.Value -> A.Parser a
jsonToAeson = textToAeson . showJSON
aesonToJson x = case readJSON $ aesonToText x of
    Ok r -> pure r
    Error e -> A.typeMismatch e x

instance A.ToJSON Board where toJSON = jsonToAeson
instance A.FromJSON Board where parseJSON = aesonToJson

instance A.ToJSON (Pos, Move) where toJSON = (A.Number . fromIntegral . enumerate)
instance A.FromJSON (Pos, Move) where 
    parseJSON (A.Number n) = return $ denumerate $ fromIntegral $ truncate n
    parseJSON x = A.typeMismatch "Expected integer" x

class Enumerable a where
    enumerate :: a -> Int
    denumerate :: Int -> a

instance JSON (Pos, Move) where
    showJSON = showJSON . enumerate
    readJSON = fmap denumerate . readJSON

instance Enumerable Move where
    enumerate (N e)          = 0 + 8 * (ptoi e)
    enumerate (PawnMove e)   = 1 + 8 * (ptoi e)
    enumerate (CharMove c e) = 2 + 8 * ((ptoi c) + 256 * (ptoi e))
    enumerate (Chain [i, e]) = 3 + 8 * ((ptoi i) + 256 * (ptoi e))
    enumerate (Chain [])     = 6
    enumerate (Chain xs)     = enumerate $ N $ head xs -- fuck you
    enumerate (Push e)       = 4 + 8 * (ptoi e)
    enumerate (Throw t e)    = 5 + 8 * (ptoi t)
    
    denumerate n = case p of
        0 -> N f
        1 -> PawnMove f
        2 -> CharMove f s
        3 -> Chain [f,s]
        4 -> Push f
        5 -> Throw f s
        6 -> Chain []
        _ -> N (0,0) -- fuck you
        where 
            p = n `div` 8    
            (q, f) = second itop $ p `divMod` 256
            s = q `divMod` 256

instance Enumerable (Pos, Move) where
    enumerate (s, move) = (ptoi s) + 256 * (enumerate move)
    denumerate n = bimap itop denumerate $ swap $ n `divMod` 256
    
instance Enumerable m => JSON m where
    showJSON = showJSON . enumerate
    readJSON = fmap denumerate . readJSON

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
intToPiece _  = Null

instance Enumerable PawnState where
    enumerate Start = 0
    enumerate Normal = 1
    enumerate Cham = 2
    enumerate (EnPassant []) = 1
    enumerate (EnPassant xs) = 3 + (ptoi $ maximum xs)

    denumerate 0 = Start
    denumerate 1 = Normal
    denumerate 2 = Cham
    denumerate n = EnPassant [(x,y), (x,y-1)] where -- This is baxany specific
        (y, x) = itop (n-3)
    

instance Enumerable PieceType where
    enumerate p@(Pawn state) = (pieceToInt p) + 64*(enumerate state)
    enumerate p@(Chameleon Null) = 60
    enumerate p@(Chameleon t) = (pieceToInt p) + 64*(enumerate t)
    enumerate p@(Ace n) = (pieceToInt p) + 64*m where
        m = if n >= 0 then n else 10-m
    enumerate piece = pieceToInt piece
    
    denumerate 60 = Chameleon Null
    denumerate n = case intToPiece $ n `mod` 64 of
        Pawn _ -> Pawn $ denumerate s
        Chameleon _ -> Chameleon $ denumerate s
        Ace _ -> Ace m         
        x -> x
        where
            s = n `div` 64
            m = if s > 10 then 10-s else s

instance Enumerable Piece where
    enumerate (Piece Black t) = 2*(enumerate t)
    enumerate (Piece White t) = 2*(enumerate t) + 1
    enumerate Block = -2
    enumerate Empty = -1
    
    denumerate (-2) = Block
    denumerate (-1) = Empty
    denumerate n = (Piece c t) where
        c = if n `mod` 2 == 0 then Black else White
        t = denumerate $ n `div` 2

