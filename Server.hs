{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.Monoid
import Data.IORef

import Board
 
main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    state <- newIORef 0
    run port (app state)
 
app state req respond = case pathInfo req of
        ["board"] -> respond baxanyResponse
        ["count"] -> incCount state respond
        x -> respond $ index x

htmlresp = [("Content-Type", "text/html")]
plainresp = [("Content-Type", "text/plain")]

incCount state respond = do
    modifyIORef state (+1)
    putStrLn "hi!"
    count <- readIORef state
    respond $ index count

baxanyResponse = responseBuilder status200 plainresp $ copyByteString $ BU.fromString $ show baxany
  
index x = responseBuilder status200 htmlresp $ mconcat $ map copyByteString
    [ "<p>Hello from ", BU.fromString $ show x, "!</p>"]
