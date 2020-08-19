{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status404, status401)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString as B
import Data.Monoid
import Data.IORef

import Text.JSON

import Json
import Board
import Move
import MoveHelper 

main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    state <- newIORef baxany
    run port (app state)

app :: (IORef Board) -> Application
app board req respond = case (requestMethod req, pathInfo req) of
        ("GET", ["board", "text"]) -> serveTextBoard board respond
        ("GET", ["board", "moves"]) -> serveBoardMoves board respond req
        ("GET", ["board"]) -> serveBoard board respond
        ("POST", ["board"]) -> respondWithMove board respond req serveBoard
        ("POST", ["board", "text"]) -> respondWithMove board respond req serveTextBoard
        ("POST", _) -> echoBody req respond
        _ -> respond $ responseBuilder status404 plainresp "Not Found"


getRequestBody :: Request -> IO BU.ByteString
getRequestBody req = do
    body <- getRequestBodyChunk req
    if B.null body  then return body
    else getRequestBody req >>= (return . (body<>))

htmlresp = [("Content-Type", "text/html")]
plainresp = [("Content-Type", "text/plain")]
jsonresp = [("Content-Type", "application/json")]

bsResponse x = (responseBuilder status200 x . copyByteString)

serveBoard state respond = do
    board <- readIORef state
    respond $ bsResponse jsonresp $ BU.fromString $ encode board

serveTextBoard state respond = do
    board <- readIORef state 
    respond $ bsResponse plainresp  $ BU.fromString $ show board

respondWithMove state respond req cb = do
    content <- getRequestBody req >>= (return . BU.toString)
    let rmove = decode content :: Result ((Int, Int), Move)
    case rmove of
        Error x -> respond $ responseBuilder status401 plainresp $ copyByteString $ BU.fromString x
        Ok (p, m) -> do
            modifyIORef' state (\b -> doMoveAt b p m)
            cb state respond
    
serveBoardMoves board respond req = do
    board <- readIORef board
    content <- getRequestBody req >>= (return . BU.toString)
    let rpos = decode content :: Result (Int, Int)
    case rpos of
        Error x -> respond $ responseBuilder status401 plainresp $ copyByteString $ BU.fromString x
        Ok pos -> respond $ bsResponse jsonresp $ BU.fromString $ encode $ movesAt board pos

echoBody req respond = do
    body <- getRequestBody req
    respond $ bsResponse plainresp body


 
