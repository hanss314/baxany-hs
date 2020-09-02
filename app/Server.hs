{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Yesod
import Yesod.Auth
import Yesod.Auth.HashDB
import Yesod.WebSockets
import qualified Yesod.Auth.Message as M
import Network.Wai
import Network.WebSockets (ConnectionException)
import Network.HTTP.Types.Header

import Data.IORef
import Data.Either
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Text.JSON
import Network.Mime
import qualified Data.Map.Strict as D
import Data.Aeson (object)

import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT)
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Monad.Trans.Reader
import qualified Control.Exception as E
import Control.Monad.IO.Unlift
import Control.Concurrent (threadDelay)

import Database.Persist
import qualified Database.Persist.Types as P (PersistUpdate(..))
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
/users              UsersR      GET POST
/admin              AdminPage   GET
/password           Password    POST
/user/#UserId       UserR       GET
/text/#GameId       BoardT      GET POST
/board/#GameId      BoardPage   GET

/board                        NewBoard    POST
/board/#GameId/json           BoardJ      GET POST
/board/#GameId/moves          MovesAt     GET
/board/#GameId/players        Players     GET
/board/#GameId/hist           BoardHist   GET
/board/#GameId/hist/last      LastHist    GET
/board/#GameId/creds          GameCreds   GET

/auth AuthR Auth getAuth

!/*Texts      File        GET
|]
         
initDB = runSqlite sqlDB $ runMigration migrateAll

instance Yesod BaxanyServer where
    authRoute _ = Just $ AuthR LoginR

    isAuthorized NewBoard _ = isAccount (/= "anyone")
    isAuthorized Password _ = isAccount (/= "anyone")
    isAuthorized UsersR True = isAccount (== "hans")
    isAuthorized AdminPage _ = isAccount (== "hans")
    isAuthorized _ _ = return Authorized

isAccount :: (T.Text -> Bool) -> Handler AuthResult
isAccount f = do
    aid <- requireAuthId
    mu <- fmap userName $ runDB $ get404 aid
    return $ if f mu then Authorized else Unauthorized "Unauthorized"

instance YesodAuth BaxanyServer where
    type AuthId BaxanyServer = UserId
    
    loginDest _ = HomeR
    logoutDest _ = HomeR
    authPlugins _ = [ authHashDB (Just . UniqueUser) ]
    authenticate creds = liftHandler $ runDB $ do
        let name = credsIdent creds
        x <- getBy $ UniqueUser name
        return $ case x of 
            Just (Entity username _) -> Authenticated username
            Nothing -> UserError $ M.IdentifierNotFound name

instance RenderMessage BaxanyServer FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAuthPersist BaxanyServer where 
    type AuthEntity BaxanyServer = User 

liftIORef = liftIO . readIORef

postNewBoard :: Handler Value
postNewBoard = do
    (whiteName, blackName) <- requireCheckJsonBody :: Handler (UserId, UserId)
    let game = initBaxanyGame whiteName blackName
    gid <- runDB $ insert400 game 
    returnJson gid

getUserR :: UserId -> Handler Value
getUserR id = do
    let filters = [GameBlack ==. id] ||. [GameWhite ==. id]
    games <- runDB $ selectList filters []
    let toObj g = object [("key" .= (entityKey g)), ("players" .= (gameBlack $ entityVal g, gameWhite $ entityVal g))]
    returnJson (id, map toObj games)

getPlayers :: GameId -> Handler Value
getPlayers id = do
    game <- runDB $ get404 id
    returnJson (gameWhite game, gameBlack game)

boardServer :: GameId -> WebSocketsT Handler ()
boardServer id = do
    chansRef <- channels <$> getYesod
    newChan <- liftIO $ newBroadcastTChanIO
    chan <- liftIO $ atomicModifyIORef' chansRef $ \m -> case m D.!? id of
        Just (i, c) -> (D.insert id (i+1, c) m, c)
        Nothing -> (D.insert id (1, newChan) m, newChan)
    
    readChan <- liftIO $ atomically $ dupTChan chan
    let sendBoard = do
            t <- liftIO $ atomically $ readTChan readChan
            sendTextData t
        ping = do
            liftIO $ threadDelay (60*1000*1000)
            sendPing ("ping" :: T.Text)

    action <- toIO $ forever $ race_ sendBoard ping
    liftIO $ E.finally action $ atomicModifyIORef' chansRef $ \m -> case m D.!? id of
        Nothing -> (m, ())
        Just (i,c) -> if i > 1 
            then (D.insert id (i-1, c) m, ())
            else (D.delete id m, ())

getBoardT :: GameId -> Handler T.Text
getBoardT id = do
    board <- fmap getDBBoard $ runDB $ get404 id
    return $ T.pack $ show $ board

getMovesAt :: GameId -> Handler Value
getMovesAt id = do
    board <- fmap getDBBoard $ runDB $ get404 id
    pos <- requireCheckJsonBody :: Handler (Int, Int)
    returnJson $ movesAt board pos

getBoardHist :: GameId -> Handler Value
getBoardHist id = do
    moveList <- fmap gameMoves $ runDB $ get404 id
    returnJson moveList

getLastHist :: GameId -> Handler Value
getLastHist id = do
    moveList <- fmap gameMoves $ runDB $ get404 id
    let resp = if null moveList then [] else [head moveList, length moveList]
    returnJson resp

getBoardJ :: GameId -> Handler Value
getBoardJ id = do
    game <- runDB $ get404 id
    returnJson $ getDBBoard game

getUsersR = do
    aid <- requireAuthId
    getUserR aid

postUsersR = do
    (name, pass) <- requireCheckJsonBody :: Handler (T.Text, T.Text)
    user <- setPassword pass $ User name Nothing
    runDB $ insert400_ user
    return name

postBoardJ = someBoardMove getBoardJ
postBoardT = someBoardMove getBoardT

getBoardPage :: GameId -> Handler ()
getBoardPage id = do
    webSockets $ boardServer id
    sendFile "text/html" "static/board.html"

getHomeR :: Handler Html
getHomeR = sendFile "text/html" "static/index.html"

getAdminPage :: Handler Html
getAdminPage = sendFile "text/html" "static/admin.html"

postPassword = do
    password <- requireCheckJsonBody :: Handler T.Text
    aid <- requireAuthId
    user <- runDB $ get404 aid
    newu <- setPassword password user
    runDB $ replace aid newu
    

getFile :: [T.Text] ->  Handler ()
getFile ts = sendFile (defaultMimeLookup fname) $ T.unpack fname where
    fname = T.intercalate "/" $ "static":ts

getGameTurn :: Game -> UserId
getGameTurn g = if ((length $ gameMoves g) `mod` 2) == 1 then (gameBlack g) else (gameWhite g)

getGameCreds :: GameId -> Handler T.Text
getGameCreds id = do
    game <- runDB $ get404 id
    anyoneid <- fmap entityKey $ runDB $ getBy404 $ UniqueUser "anyone"
    aid <- fromMaybe anyoneid <$> maybeAuthId
    let whiteId = gameWhite game
    let blackId = gameBlack game
    let white = if whiteId == aid || whiteId == anyoneid then 1 else 0
    let black = if blackId == aid || blackId == anyoneid then 2 else 0
    return $ T.pack $ show (white + black)

someBoardMove :: (GameId -> Handler a) -> GameId -> Handler a
someBoardMove callback id = do
    game <- runDB $ get404 id
    (pos, move) <- requireCheckJsonBody :: Handler (Pos, Move)
    anyoneid <- fmap entityKey $ runDB $ getBy404 $ UniqueUser "anyone"
    aid <- fromMaybe anyoneid <$> maybeAuthId
    let turn = getGameTurn game
    mu <- fmap userName $ runDB $ get404 turn
    unless (aid==turn || turn==anyoneid) $ permissionDenied "You may not play here"
    let board = getDBBoard game
    let moves = map denumerate $ gameMoves game :: [(Pos, Move)]
    case doMoveAt board pos move of
        Left err -> permissionDenied $ T.pack err
        Right b -> do
            chans <- getYesod >>= (liftIORef . channels)
            case chans D.!? id of
                Nothing -> return ()
                Just (_, chan) -> liftIO $ atomically $ writeTChan chan $ T.pack $ encode (b, ((pos, move), length moves + 1))
            let updateBoard = Update GameBoard $ enumerateBoard b
            let updateMoves = Update GameMoves $ map enumerate $ (pos,move) : moves
            runDB $ update id [updateBoard P.Assign, updateMoves P.Assign]
            callback id

main = do
    let port = 3000
    server <- initServer
    putStrLn $ "Listening on port " ++ show port
    runNoLoggingT $ withSqlitePool sqlDB 10 $ \pool -> liftIO $ do
        initDB
        warp port (server pool)

