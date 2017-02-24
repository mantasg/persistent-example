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
    firstName String
    lastName String
    age Int
    PersonName firstName lastName
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
    
    insert $ Person "Michael" "Snoyman" 26
    insert $ Person "Another" "Guy" 100
    michael <- getBy $ PersonName "Michael" "Snoyman"
    liftIO $ print michael
    
    personById <- get $ PersonKey 1
    liftIO $ print $ "Person by ID = " ++ show personById
    
    maybePerson <- getBy $ PersonName "Michael" "Snoyman"
    liftIO $ print $ "Person by Name = " ++ show maybePerson
    
    people <- selectList [PersonAge >. 25] [ Asc PersonAge ]
    liftIO $ print people
    
    carId <- insert $ Car "Red" "Honda" "Civic"
    car <- get carId
    liftIO $ print car