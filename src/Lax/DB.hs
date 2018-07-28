{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Lax.DB where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT)

import Database.Persist
import Database.Persist.Postgresql

import Lax.DB.Models

import Database.PostgreSQL.Simple (postgreSQLConnectionString, defaultConnectInfo, ConnectInfo(..))


connInfo = defaultConnectInfo { connectHost = "localhost"
                              , connectPort = 5432
                              , connectDatabase = "lax"
                              , connectUser = "postgres"
                              , connectPassword = "pass"
                              }

connString :: ConnectionString
connString = postgreSQLConnectionString connInfo

runDB :: SqlPersistT IO a -> IO a
runDB query =
  runStderrLoggingT $
  withPostgresqlPool connString 10 $ \pool -> liftIO $ runSqlPool query pool

initDB :: IO ()
initDB = runDB $ runMigration migrateAll
