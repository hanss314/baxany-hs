{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status404, status401)
import Network.HTTP.Types.Header (hAuthorization)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as B (c2w, w2c)
import Data.Monoid
import Data.IORef
import Data.Either
import System.IO
import System.Environment
import qualified Data.Text as T
import Text.JSON
import Network.Mime
import System.Random

import Json
import Board
import Move
import MoveHelper 
import Piece

main = do
    let port = 3000
    state <- newIORef baxany
    moves <- newIORef []
    args <- getArgs
    let shouldAuth = "auth" `elem` args
    blackAuth <- (newStdGen >>= return . take 100 . randomRs ('0', 'Z'))
    whiteAuth <- (newStdGen >>= return . take 100 . randomRs ('0', 'Z'))
    putStrLn $ "Black: " ++ blackAuth
    putStrLn $ "White: " ++ whiteAuth
    putStrLn $ "Listening on port " ++ show port
    run port (app state moves (blackAuth, whiteAuth) shouldAuth)

app :: (IORef Board) -> (IORef [((Int, Int), Move)]) -> (String, String) -> Bool -> Application
app board moves auth shouldAuth req respond = case (requestMethod req, pathInfo req) of
        ("GET", ["board", "text"]) -> serveTextBoard board respond
        ("GET", ["board", "moves"]) -> serveBoardMoves board respond req
        ("GET", ["board", "hist"]) -> serveAllMoves moves respond
        ("GET", ["board", "hist", "last"]) -> serveLastMove moves respond
        ("GET", ["board", "json"]) -> serveBoard board respond
        
        ("POST", ["board"]) -> respWithMove serveBoard
        ("POST", ["board", "text"]) -> respWithMove serveTextBoard
        
        ("GET", ["board", "needauth"]) -> respond $ bsResponse jsonresp $ BU.fromString $ encode shouldAuth

        ("GET", ["board"]) -> respond $ responseFile status200 htmlresp "static/index.html" Nothing
        ("GET", "board":xs) -> serveFile respond $ T.intercalate "/" $ "static":xs
        ("POST", _) -> echoBody req respond
        _ -> respond $ responseBuilder status404 plainresp "Not Found"
        where respWithMove = ((if shouldAuth then doAuth auth req board respond else id) 
                              . respondWithMove board moves respond req)

htmlresp = [("Content-Type", "text/html")]
plainresp = [("Content-Type", "text/plain")]
jsonresp = [("Content-Type", "application/json")]

bsResponse x = (responseBuilder status200 x . copyByteString)

doAuth (b, w) req state respond x = do
    let headers = requestHeaders req
    let authheaders = filter ((==hAuthorization) . fst) headers
    let auth = if null authheaders then "" else snd $ head authheaders
    board <- readIORef state
    let validate = if turn board == Black then b else w
    if auth == BU.fromString validate then x 
    else respond $ responseBuilder status401 plainresp "Unauthorized"

readReqContent req = strictRequestBody req >>= (return . map B.w2c . BL.unpack)

serveFile respond filename = respond $ responseFile status200 ct (T.unpack $ filename) Nothing
    where ct = [("Content-Type", defaultMimeLookup filename)]

serveAllMoves state respond = do
    moves <- readIORef state
    respond $ bsResponse jsonresp $ BU.fromString $ encode moves

serveLastMove state respond = do
    moves <- readIORef state
    let resp = if null moves then "[]" else encode $ (head moves, length moves)
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
    print rmove
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

