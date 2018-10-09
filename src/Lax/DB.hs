{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Lax.DB where

import Data.Text.Lazy (Text, toStrict)
import Data.ByteString (ByteString)
import Text.RawString.QQ

import Data.Text.Encoding (encodeUtf8)

import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT)

import Crypto.BCrypt (hashPasswordUsingPolicy, fastBcryptHashingPolicy, slowerBcryptHashingPolicy, validatePassword)

import Database.Persist
import Database.Persist.Postgresql

import Data.Aeson

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

createTriggers :: ReaderT SqlBackend IO ()
createTriggers = rawExecute q []
  where q = [r|
              CREATE OR REPLACE FUNCTION new_user_notify() RETURNS TRIGGER AS $$
                DECLARE
                  payload varchar;
                  un varchar;
                BEGIN
                  SELECT username INTO un FROM users WHERE id = NEW.id;
                  payload = un;
                  PERFORM pg_notify('new_user', payload);
                  RETURN NEW;
                END;
              $$ LANGUAGE plpgsql;

              DROP TRIGGER IF EXISTS user_insert ON users;
              CREATE TRIGGER user_insert
              AFTER INSERT
              ON users
              FOR EACH ROW
                EXECUTE PROCEDURE new_user_notify();
              |]

initDB :: SqlPersistT IO ()
initDB = runMigration migrateAll

-- security
-- NOTE: using fast policy ONLY for development
hashPassword :: Text -> IO (Maybe ByteString)
hashPassword = hashPasswordUsingPolicy fastBcryptHashingPolicy . encodeUtf8 . toStrict

-- database interaction
-- addUser :: 

instance ToJSON User where
  toJSON (User un em _) =
    object ["username" .= un, "email" .= em]
