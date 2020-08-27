{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

import Yesod
import Network.Wai
import Network.HTTP.Types.Header

import Data.Monoid
import Data.IORef
import Data.Either
import System.IO
import System.Environment
import Data.String
import qualified Data.Text as T
import Text.JSON
import Network.Mime
import System.Random

import Json
import Board
import Move
import MoveHelper 
import Piece

data BaxanyServer = BaxanyServer 
                  { board :: IORef Board
                  , moves :: IORef [((Int, Int), Move)]
                  , auth :: (String, String)
                  , shouldAuth :: Bool
                  }

mkYesod "BaxanyServer" [parseRoutes|
/board/text         BoardT      GET POST
/board              BoardM      GET POST
/board/json         BoardJ      GET
/board/moves        MovesAt     GET
/board/hist         BoardHist   GET
/board/hist/last    LastHist    GET
/board/needauth/    RequireAuth GET
!/board/*Texts      File        GET
|]
         
instance Yesod BaxanyServer

liftIORef = liftIO . readIORef

getBoardT = do
    boardRef <- getYesod >>= (liftIORef . board)
    return $ T.pack $ show boardRef

getMovesAt = do
    boardRef <- getYesod >>= (liftIORef . board)
    pos <- requireCheckJsonBody :: Handler (Int, Int)
    return $ T.pack $ encode $ movesAt boardRef pos

getBoardHist = do
    movesRef <- getYesod >>= (liftIORef . moves)
    return $ T.pack $ encode $ movesRef

getLastHist = do
    movesRef <- getYesod >>= (liftIORef . moves)
    let resp = if null movesRef then "[]" else encode $ (head movesRef, length movesRef)
    return $ T.pack $ resp

getBoardJ = do
    boardRef <- getYesod >>= (liftIORef . board)
    return $ T.pack $ encode $ boardRef

getRequireAuth = do
    yesod <- getYesod 
    return $ T.pack $ encode $ shouldAuth yesod

postBoardM = someBoardMove getBoardJ
postBoardT = someBoardMove getBoardT

getBoardM :: Handler () 
getBoardM = sendFile "text/html" "static/index.html"

getFile :: [T.Text] ->  Handler ()
getFile ts = sendFile (defaultMimeLookup fname) $ T.unpack fname where
    fname = T.intercalate "/" $ "static":ts

someBoardMove callback = do  
    yesod <- getYesod
    (pos, move) <- requireCheckJsonBody :: Handler ((Int, Int), Move)
    let reqAuth = shouldAuth yesod
    let boardRef = board yesod
    let authKeys = auth yesod
    request <- getRequest
    let authheaders = filter ((==hAuthorization) . fst) $ requestHeaders $ reqWaiRequest $ request
    let userAuth = if null authheaders then "" else snd $ head authheaders
    boardObj <- liftIORef boardRef    
    let authSide = if turn boardObj == Black then fst authKeys else snd authKeys
    if reqAuth && (fromString authSide /= userAuth) then notAuthenticated
    else do
        result <- liftIO $ atomicModifyIORef' boardRef (\b -> (fromRight b $ doMoveAt b pos move, doMoveAt b pos move))
        case result of
            Left err -> permissionDenied $ T.pack err
            Right _ -> do
                liftIO $ atomicModifyIORef' (moves yesod) (\ms -> ((pos, move):ms, ()))
                callback

main = do
    let port = 3000
    board <- newIORef baxany
    moves <- newIORef []
    args <- getArgs
    let shouldAuth = "auth" `elem` args
    blackAuth <- (newStdGen >>= return . take 100 . randomRs ('0', 'Z'))
    whiteAuth <- (newStdGen >>= return . take 100 . randomRs ('0', 'Z'))
    putStrLn $ "Black: " ++ blackAuth
    putStrLn $ "White: " ++ whiteAuth
    putStrLn $ "Listening on port " ++ show port
    warp port (BaxanyServer board moves (blackAuth, whiteAuth) shouldAuth)

