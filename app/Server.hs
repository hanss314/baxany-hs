{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Yesod
import Yesod.Auth
import Yesod.Auth.HashDB
import Yesod.WebSockets
import qualified Yesod.Auth.Message as M
import Network.Wai
import Network.HTTP.Types.Header

import Data.IORef
import Data.Either
import Data.String
import qualified Data.Text as T
import Text.JSON
import Network.Mime
import qualified Data.Map.Strict as D

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Monad.Trans.Reader

import Database.Persist
import Database.Persist.Sqlite

import BaxanyServer
import BaxanyDB

import Json
import Board
import Move
import MoveHelper 
import Piece
import Pos

instance HashDBUser User where
    userPasswordHash = userPassword
    setPasswordHash h u = u { userPassword = (Just h) }

mkYesod "BaxanyServer" [parseRoutes|
/                   HomeR       GET
/users              UsersR      POST
/user/#UserId       UserR       GET
/text/#DBGameId     BoardT      GET POST
/board/#DBGameId    BoardPage   GET

/board                          NewBoard    POST
/board/#DBGameId/json           BoardJ      GET POST
/board/#DBGameId/moves          MovesAt     GET
/board/#DBGameId/hist           BoardHist   GET
/board/#DBGameId/hist/last      LastHist    GET

/auth AuthR Auth getAuth

!/*Texts      File        GET
|]
         
initDB = runSqlite sqlDB $ runMigration migrateAll

instance Yesod BaxanyServer

instance YesodAuth BaxanyServer where
    type AuthId BaxanyServer = UserId
    
    loginDest _ = HomeR
    logoutDest _ = HomeR
    authPlugins _ = [ authHashDB (Just . UniqueUser) ]
    authenticate creds = liftHandler $ runDB $ do
        let name = credsIdent creds
        x <- getBy $ UniqueUser name
        liftIO $ print x
        return $ case x of 
            Just (Entity username _) -> Authenticated username
            Nothing -> UserError $ M.IdentifierNotFound name

instance RenderMessage BaxanyServer FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAuthPersist BaxanyServer where 
    type AuthEntity BaxanyServer = User 

createGameFromId :: IORef (D.Map DBGameId Game) -> DBGameId -> HandlerFor BaxanyServer Game
createGameFromId list id = do
    dbg <- runDB $ get404 id
    game <- liftIO $ createGameFromDB dbg
    liftIO $ atomicModifyIORef' list (\m -> (D.insert id game m, ()))
    return game

getGame id = do
    yesod <- getYesod
    gameList <- liftIORef $ games yesod
    case gameList D.!? id of
        Just game -> return game
        Nothing -> createGameFromId (games yesod) id

liftIORef = liftIO . readIORef

postNewBoard = do
    (whiteName, blackName) <- requireCheckJsonBody :: Handler (UserId, UserId)
    game <- liftIO $ initBaxanyGame whiteName blackName
    gid <- runDB $ insert400 $ gameToDB game 
    gamesRef <- games <$> getYesod
    liftIO $ atomicModifyIORef' gamesRef (\m -> (D.insert gid game m, ()))
    return $ show gid

getUserR :: UserId -> Handler T.Text
getUserR id = do
    user <- runDB $ get404 id
    return $ T.pack $ show user

boardServer :: Game -> WebSocketsT Handler ()
boardServer game = do
    let chan = channel game
    readChan <- liftIO $ atomically $ dupTChan chan
    forever $ do
        t <- liftIO $ atomically $ readTChan readChan
        sendTextData t

getBoardT id = do
    game <- getGame id
    return $ T.pack $ show $ board game

getMovesAt id = do
    boardRef <- board <$> getGame id
    pos <- requireCheckJsonBody :: Handler (Int, Int)
    return $ T.pack $ encode $ movesAt boardRef pos

getBoardHist id = do
    moveList <- moves <$> getGame id
    return $ T.pack $ encode $ moveList

getLastHist id = do
    moveList <- moves <$> getGame id
    let resp = if null moveList then "[]" else encode $ (head moveList, length moveList)
    return $ T.pack $ resp

getBoardJ id = do
    game <- getGame id
    return $ T.pack $ encode $ board game

getUsers = do
    name <- requireCheckJsonBody :: Handler T.Text
    user <- runDB $ getBy404 $ UniqueUser name
    return $ show user

postUsersR = do
    (name, pass) <- requireCheckJsonBody :: Handler (T.Text, T.Text)
    user <- setPassword pass $ User name Nothing
    runDB $ insert400_ user
    return name

postBoardJ = someBoardMove getBoardJ
postBoardT = someBoardMove getBoardT

getBoardPage :: DBGameId -> Handler ()
getBoardPage id = do
    game <- getGame id
    webSockets $ boardServer game
    sendFile "text/html" "static/index.html"

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    defaultLayout
        [whamlet|
            <p>Your current auth ID: #{show maid}
            $maybe _ <- maid
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>Go to the login page
        |]

getFile :: [T.Text] ->  Handler ()
getFile ts = sendFile (defaultMimeLookup fname) $ T.unpack fname where
    fname = T.intercalate "/" $ "static":ts

moveGame :: DBGameId -> (Pos, Move) -> D.Map DBGameId Game -> (D.Map DBGameId Game, Either String Board)
moveGame id (pos, move) m = case m D.!? id of
    Nothing -> (m, Left "Board not found")
    Just game -> case doMoveAt (board game) pos move of
        Left err -> (m, Left err)
        Right board -> (D.insert id (game {board=board, moves = (pos, move):(moves game)}) m, Right board)

getGameTurn :: Game -> UserId
getGameTurn g = if (turn $ board g) == Black then fst $ players g else snd $ players g 

someBoardMove callback id = do  
    game <- getGame id
    gameList <- games <$> getYesod
    (pos, move) <- requireCheckJsonBody :: Handler (Pos, Move)
    let turnPlayer = getGameTurn game
    maid <- maybeAuthId
    unless (maybe False (==turnPlayer) maid) $ permissionDenied "You may not play here"
    result <- liftIO $ atomicModifyIORef' gameList $ moveGame id (pos, move)
    case result of
        Left err -> permissionDenied $ T.pack err
        Right b -> do
            liftIO $ atomically $ writeTChan (channel game) $ T.pack $ encode b
            callback id

main = do
    let port = 3000
    server <- initServer
    putStrLn $ "Listening on port " ++ show port
    runStderrLoggingT $ withSqlitePool sqlDB 10 $ \pool -> liftIO $ do
        initDB
        warp port (server pool)

