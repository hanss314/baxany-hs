{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving, StandaloneDeriving, UndecidableInstances #-}

module BaxanyDB where

import Data.Text (Text)
import Database.Persist.TH

sqlDB :: Text
sqlDB = "baxany.sqlite3"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Text
    password Text Maybe
    Primary name
    UniqueUser name
    deriving Show
Game
    board [Int]
    moves [Int]
    black UserId
    white UserId
    deriving Show
|]

