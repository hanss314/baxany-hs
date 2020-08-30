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
/users              UsersR      POST
/user/#UserId       UserR       GET
/text/#GameId       BoardT      GET POST
/board/#GameId      BoardPage   GET

/board                        NewBoard    POST
/board/#GameId/json           BoardJ      GET POST
/board/#GameId/moves          MovesAt     GET
/board/#GameId/hist           BoardHist   GET
/board/#GameId/hist/last      LastHist    GET

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

liftIORef = liftIO . readIORef

postNewBoard = do
    (whiteName, blackName) <- requireCheckJsonBody :: Handler (UserId, UserId)
    let game = initBaxanyGame whiteName blackName
    gid <- runDB $ insert400 game 
    return $ show gid

getUserR :: UserId -> Handler T.Text
getUserR id = do
    user <- runDB $ get404 id
    return $ T.pack $ show user

boardServer :: GameId -> WebSocketsT Handler ()
boardServer id = do
    chansRef <- channels <$> getYesod
    chan <- liftIO $ join $ atomically $ return $ do
        chans <- readIORef chansRef
        case chans D.!? id of
            Just c -> return c
            Nothing -> do
                newChan <- newBroadcastTChanIO
                atomicModifyIORef' chansRef (\m -> (D.insert id newChan m, newChan))

    readChan <- liftIO $ atomically $ dupTChan chan
    forever $ do
        t <- liftIO $ atomically $ readTChan readChan
        sendTextData t

getBoardT :: GameId -> Handler T.Text
getBoardT id = do
    board <- fmap getDBBoard $ runDB $ get404 id
    return $ T.pack $ show $ board

getMovesAt :: GameId -> Handler T.Text
getMovesAt id = do
    board <- fmap getDBBoard $ runDB $ get404 id
    pos <- requireCheckJsonBody :: Handler (Int, Int)
    return $ T.pack $ encode $ movesAt board pos

getBoardHist :: GameId -> Handler T.Text
getBoardHist id = do
    moveList <- fmap gameMoves $ runDB $ get404 id
    return $ T.pack $ encode $ moveList

getLastHist :: GameId -> Handler T.Text
getLastHist id = do
    moveList <- fmap gameMoves $ runDB $ get404 id
    let resp = if null moveList then "[]" else encode $ (head moveList, length moveList)
    return $ T.pack $ resp

getBoardJ :: GameId -> Handler T.Text
getBoardJ id = do
    game <- runDB $ get404 id
    return $ T.pack $ encode $ getDBBoard game

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

getBoardPage :: GameId -> Handler ()
getBoardPage id = do
    webSockets $ boardServer id
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


getGameTurn :: Game -> UserId
getGameTurn g = if (length $ gameBoard g) `mod` 2 == 1 then gameBlack g else gameWhite g 

someBoardMove callback id = do
    game <- runDB $ get404 id
    (pos, move) <- requireCheckJsonBody :: Handler (Pos, Move)
    maid <- maybeAuthId
    unless (maybe False (==getGameTurn game) maid) $ permissionDenied "You may not play here"
    let board = getDBBoard game
    let moves = map denumerate $ gameMoves game :: [(Pos, Move)]
    case doMoveAt board pos move of
        Left err -> permissionDenied $ T.pack err
        Right b -> do
            chans <- getYesod >>= (liftIORef . channels)
            case chans D.!? id of
                Nothing -> return ()
                Just chan -> liftIO $ atomically $ writeTChan chan $ T.pack $ encode (b, ((pos, move), length moves + 1))
            let updateBoard = Update GameBoard $ enumerateBoard b
            let updateMoves = Update GameMoves $ map enumerate $ (pos,move) : moves
            runDB $ update id [updateBoard P.Assign, updateMoves P.Assign]
            callback id

main = do
    let port = 3000
    server <- initServer
    putStrLn $ "Listening on port " ++ show port
    runStderrLoggingT $ withSqlitePool sqlDB 10 $ \pool -> liftIO $ do
        initDB
        warp port (server pool)

