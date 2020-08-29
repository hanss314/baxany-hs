module BaxanyServer where

import Data.IORef
import Data.Text (Text, unpack, pack)
import Control.Concurrent.STM.TChan
import Control.Monad.STM (atomically)
import Database.Persist.Sqlite (ConnectionPool)
import Yesod
import Data.Map.Strict (Map, empty)
import Text.JSON

import Json
import Board
import MoveHelper
import Pos
import BaxanyDB

data Game = Game { board :: Board
                 , moves :: [(Pos, Move)]
                 , channel :: TChan Text
                 , players :: (UserId, UserId)
                 }

data BaxanyServer = BaxanyServer 
                  { games :: IORef (Map DBGameId Game)
                  , pool :: ConnectionPool
                  }

initBaxanyGame :: UserId -> UserId -> IO Game
initBaxanyGame white black = do
    channel <- atomically $ newBroadcastTChan 
    return $ Game baxany [] channel (black, white)

initServer :: IO (ConnectionPool -> BaxanyServer)
initServer = do
    games <- newIORef empty
    return $ BaxanyServer games

gameToDB :: Game -> DBGame
gameToDB (Game b m _ (bl, wh)) = DBGame (pack $ encode b) (pack $ encode m) bl wh

parseDBGameJSON :: DBGame -> Result (TChan Text -> (UserId, UserId) -> Game)
parseDBGameJSON dbg = do
    board <- decode $ unpack $ dBGameBoard dbg
    moves <- decode $ unpack $ dBGameMoves dbg
    return $ Game board moves

createGameFromDB :: DBGame -> IO Game
createGameFromDB dbg = do
    channel <- atomically $ newBroadcastTChan 
    let players = (dBGameBlack dbg, dBGameWhite dbg)
    return $ case parseDBGameJSON dbg of
        Ok result -> result channel players
        Error _ -> Game baxany [] channel players
