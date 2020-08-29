{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving, StandaloneDeriving, UndecidableInstances #-}

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

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Monad.Trans.Reader

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import BaxanyServer

import Json
import Board
import Move
import MoveHelper 
import Piece

sqlDB :: T.Text
sqlDB = "baxany.sqlite3"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name T.Text
    password T.Text Maybe
    UniqueUser name
    deriving Show
|]

instance HashDBUser User where
    userPasswordHash = userPassword
    setPasswordHash h u = u { userPassword = (Just h) }

mkYesod "BaxanyServer" [parseRoutes|
/                   HomeR       GET
/board              BoardPage   GET
/text               BoardT      GET POST
/api/board          BoardJ      GET POST
/api/moves          MovesAt     GET
/api/hist           BoardHist   GET
/api/hist/last      LastHist    GET
/api/needauth/      RequireAuth GET

/api/users          Users       GET POST
/auth AuthR Auth getAuth

!/*Texts      File        GET
|]
         
initDB = runSqlite sqlDB $ runMigration migrateAll

instance Yesod BaxanyServer

instance YesodPersist BaxanyServer where
    type YesodPersistBackend BaxanyServer = SqlBackend
    runDB action = do
        server <- getYesod
        runSqlPool action $ pool server

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

boardServer :: WebSocketsT Handler ()
boardServer = do
    chan <- channel <$> getYesod
    readChan <- liftIO $ atomically $ dupTChan chan
    forever $ do
        t <- liftIO $ atomically $ readTChan readChan
        sendTextData t

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
    user <- runDB $ getBy404 $ UniqueUser name
    return $ show user

postUsers = do
    (name, pass) <- requireCheckJsonBody :: Handler (T.Text, T.Text)
    user <- setPassword pass $ User name Nothing
    runDB $ insert400 user
    return name

postBoardJ = someBoardMove getBoardJ
postBoardT = someBoardMove getBoardT

getBoardPage :: Handler ()
getBoardPage = do
    sendFile "text/html" "static/index.html"

getHomeR :: Handler Html
getHomeR = do
    webSockets boardServer
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
            Right b -> do
                liftIO $ atomically $ writeTChan (channel yesod) $ T.pack $ encode b
                liftIO $ atomicModifyIORef' (moves yesod) (\ms -> ((pos, move):ms, ()))
                callback

main = do
    let port = 3000
    server <- initServer
    putStrLn $ "Listening on port " ++ show port
    runStderrLoggingT $ withSqlitePool sqlDB 10 $ \pool -> liftIO $ do
        initDB
        warp port (server pool)

