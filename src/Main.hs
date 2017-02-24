{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int
    deriving Show
Car
    color String
    make String
    model String
    deriving Show
|]


main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll
    
    michaelId <- insert $ Person "Michael" 26
    michael <- get michaelId
    liftIO $ print michael
    
    carId <- insert $ Car "Red" "Honda" "Civic"
    car <- get carId
    liftIO $ print car
