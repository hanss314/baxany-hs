{-# LANGUAGE TypeFamilies #-} 

module BaxanyServer where

import Data.IORef
import Data.Text (Text, unpack, pack)
import Control.Concurrent.STM.TChan
import Control.Monad.STM (atomically)
import Database.Persist.Sqlite 
import Yesod
import Data.Map.Strict (Map, empty)
import Text.JSON
import qualified Data.Vector as V

import Json
import Board
import MoveHelper
import Pos
import BaxanyDB
import Piece (Color(..))

data BaxanyServer = BaxanyServer 
                  { channels :: IORef (Map GameId (TChan Text))
                  , pool :: ConnectionPool
                  }

initBaxanyGame :: UserId -> UserId -> Game
initBaxanyGame white black = Game (enumerateBoard baxany) [] black white

enumerateBoard :: Board -> [Int]
enumerateBoard b = map enumerate $ V.toList $ toVector b

initServer :: IO (ConnectionPool -> BaxanyServer)
initServer = do
    games <- newIORef empty
    return $ BaxanyServer games

getDBBoard :: Game -> Board
getDBBoard dbg = Board { size = 16
                       , turn = if ((length $ gameMoves dbg) `mod` 2) == 0 then White else Black
                       , toVector = V.fromList $ map denumerate $ gameBoard dbg
                       }

instance YesodPersist BaxanyServer where
    type YesodPersistBackend BaxanyServer = SqlBackend
    runDB action = do
        server <- getYesod
        runSqlPool action $ pool server
