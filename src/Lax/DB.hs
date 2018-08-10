{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Lax.DB where

import Data.ByteString (ByteString)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT)

import Crypto.BCrypt (hashPasswordUsingPolicy, fastBcryptHashingPolicy, slowerBcryptHashingPolicy, validatePassword)

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

-- runDB :: SqlPersistT IO a -> IO a
-- runDB query =
--   runStderrLoggingT $
--   withPostgresqlPool connString 10 $ \pool -> liftIO $ runSqlPool query pool

initDB :: SqlPersistT IO ()
initDB = runMigration migrateAll

-- security
-- NOTE: using fast policy ONLY for development
hashPassword :: ByteString -> IO (Maybe ByteString)
hashPassword = hashPasswordUsingPolicy fastBcryptHashingPolicy

-- database interaction
-- addUser :: 
