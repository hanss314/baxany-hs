{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving, StandaloneDeriving, UndecidableInstances #-}

import Yesod
import Network.Wai
import Network.HTTP.Types.Header

import Data.IORef
import Data.Either
import System.IO
import System.Environment
import Data.String
import qualified Data.Text as T
import Text.JSON
import Network.Mime
import System.Random

import Control.Monad.Logger (runStderrLoggingT)

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Json
import Board
import Move
import MoveHelper 
import Piece

sqlDB :: T.Text
sqlDB = "baxany.sqlite3"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    username T.Text
    passwordHash T.Text
    UniqueUsername username
    deriving Show
|]

data BaxanyServer = BaxanyServer 
                  { board :: IORef Board
                  , moves :: IORef [((Int, Int), Move)]
                  , auth :: (String, String)
                  , shouldAuth :: Bool
                  , pool :: ConnectionPool
                  }

mkYesod "BaxanyServer" [parseRoutes|
/                   MainPage    GET
/text               BoardT      GET POST
/api/board          BoardJ      GET POST
/api/moves          MovesAt     GET
/api/hist           BoardHist   GET
/api/hist/last      LastHist    GET
/api/needauth/      RequireAuth GET

/api/users          Users       GET POST

!/*Texts      File        GET
|]
         

initDB = runSqlite sqlDB $ runMigration migrateAll

instance Yesod BaxanyServer

instance YesodPersist BaxanyServer where
    type YesodPersistBackend BaxanyServer = SqlBackend
    runDB action = do
        server <- getYesod
        runSqlPool action $ pool server

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

getUsers = do
    name <- requireCheckJsonBody :: Handler T.Text
    user <- runDB $ getBy404 $ UniqueUsername name
    return $ show user

postUsers = do
    (name, pass) <- requireCheckJsonBody :: Handler (T.Text, T.Text)
    runDB $ insert400_ $ User name pass
    return name

postBoardJ = someBoardMove getBoardJ
postBoardT = someBoardMove getBoardT

getMainPage :: Handler () 
getMainPage = sendFile "text/html" "static/index.html"

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

initServer :: IO (ConnectionPool -> BaxanyServer)
initServer = do
    board <- newIORef baxany
    moves <- newIORef []
    args <- getArgs
    let shouldAuth = "auth" `elem` args
    blackAuth <- (newStdGen >>= return . take 100 . randomRs ('0', 'Z'))
    whiteAuth <- (newStdGen >>= return . take 100 . randomRs ('0', 'Z'))
    putStrLn $ "Black: " ++ blackAuth
    putStrLn $ "White: " ++ whiteAuth
    return $ BaxanyServer board moves (blackAuth, whiteAuth) shouldAuth

main = do
    let port = 3000
    server <- initServer
    putStrLn $ "Listening on port " ++ show port
    runStderrLoggingT $ withSqlitePool sqlDB 10 $ \pool -> liftIO $ do
        initDB
        warp port (server pool)

