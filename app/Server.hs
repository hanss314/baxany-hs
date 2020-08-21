{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status404, status401)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as B (c2w, w2c)
import Data.Monoid
import Data.IORef
import Data.Either
import System.IO
import qualified Data.Text as T
import Text.JSON
import Network.Mime

import Json
import Board
import Move
import MoveHelper 

main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    state <- newIORef baxany
    moves <- newIORef []
    run port (app state moves)

app :: (IORef Board) -> (IORef [((Int, Int), Move)]) -> Application
app board moves req respond = case (requestMethod req, pathInfo req) of
        ("GET", ["board", "text"]) -> serveTextBoard board respond
        ("GET", ["board", "moves"]) -> serveBoardMoves board respond req
        ("GET", ["board", "hist"]) -> serveAllMoves moves respond
        ("GET", ["board", "hist", "last"]) -> serveLastMove moves respond
        ("GET", ["board", "json"]) -> serveBoard board respond
        
        ("POST", ["board"]) -> respondWithMove board moves respond req serveBoard
        ("POST", ["board", "text"]) -> respondWithMove board moves respond req serveTextBoard
        
        ("GET", ["board"]) -> respond $ responseFile status200 htmlresp "static/index.html" Nothing
        ("GET", "board":xs) -> serveFile respond $ T.intercalate "/" $ "static":xs
        ("POST", _) -> echoBody req respond
        _ -> respond $ responseBuilder status404 plainresp "Not Found"

htmlresp = [("Content-Type", "text/html")]
plainresp = [("Content-Type", "text/plain")]
jsonresp = [("Content-Type", "application/json")]

bsResponse x = (responseBuilder status200 x . copyByteString)

readReqContent req = strictRequestBody req >>= (return . map B.w2c . BL.unpack)

serveFile respond filename = respond $ responseFile status200 ct (T.unpack $ filename) Nothing
    where ct = [("Content-Type", defaultMimeLookup filename)]

serveAllMoves state respond = do
    moves <- readIORef state
    respond $ bsResponse jsonresp $ BU.fromString $ encode moves

serveLastMove state respond = do
    moves <- readIORef state
    let resp = if null moves then "[]" else encode $ head moves
    respond $ bsResponse jsonresp $ BU.fromString $ resp

serveBoard state respond = do
    board <- readIORef state
    respond $ bsResponse jsonresp $ BU.fromString $ encode board

serveTextBoard state respond = do
    board <- readIORef state 
    respond $ bsResponse plainresp  $ BU.fromString $ show board

respondWithMove state smoves respond req cb = do
    content <- readReqContent req
    let rmove = decode content :: Result ((Int, Int), Move)
    case rmove of
        Error x -> respond $ responseBuilder status401 plainresp $ copyByteString $ BU.fromString x
        Ok move@(p, m) -> do
            result <- atomicModifyIORef' state (\b -> (fromRight b $ doMoveAt b p m, doMoveAt b p m))
            if isLeft result 
            then respond $ responseBuilder status401 plainresp $ copyByteString $ BU.fromString $ fromLeft "" result
            else do 
                atomicModifyIORef' smoves (\ms -> (move:ms, ()))
                cb state respond
    
serveBoardMoves board respond req = do
    board <- readIORef board
    content <- readReqContent req
    let rpos = decode content :: Result (Int, Int)
    case rpos of
        Error x -> respond $ responseBuilder status401 plainresp $ copyByteString $ BU.fromString x
        Ok pos -> respond $ bsResponse jsonresp $ BU.fromString $ encode $ movesAt board pos

echoBody req respond = do
    body <- strictRequestBody req
    respond $ bsResponse plainresp $ BL.toStrict body

