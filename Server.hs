{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status404)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString as B
import Data.Monoid
import Data.IORef

import Board
 
main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    state <- newIORef baxany
    run port (app state)

app :: (IORef Board) -> Application
app board req respond = case (requestMethod req, pathInfo req) of
        ("GET", ["board"]) -> serveBoard board respond
        ("GET", x)         -> respond $ index x
        ("POST", _) -> echoBody req respond
        _ -> respond $ responseBuilder status404 plainresp "Not Found"


getRequestBody :: Request -> IO BU.ByteString
getRequestBody req = do
    body <- getRequestBodyChunk req
    if B.null body  then return body
    else getRequestBody req >>= (return . (body<>))

htmlresp = [("Content-Type", "text/html")]
plainresp = [("Content-Type", "text/plain")]

serveBoard state respond = do
    board <- readIORef state 
    respond $ responseBuilder status200 plainresp $ copyByteString $ BU.fromString $ show board

echoBody req respond = do
    body <- getRequestBody req
    respond $ responseBuilder status200 plainresp $ copyByteString body


 
index x = responseBuilder status200 htmlresp $ mconcat $ map copyByteString
    [ "<p>Hello from ", BU.fromString $ show x, "!</p>"]
