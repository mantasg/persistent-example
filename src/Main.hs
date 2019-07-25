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
import           Control.Monad.Logger    (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.Postgresql
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

withSqlite :: IO ()
withSqlite = runSqlite ":memory:" $ do
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


-- In order for get this working you need pg_config in your path
-- For that you need to download and install posgres locally
connStr = "host=localhost dbname=postgres user=postgres password=postgres port=5432"

withPostgres :: IO ()
withPostgres = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll

        insert $ Car "Red" "Honda" "Civic"
        records <- selectList [] [ Asc CarMake ]
        liftIO $ print records

        return ()

main :: IO ()
main = withSqlite