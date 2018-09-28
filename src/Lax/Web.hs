{-# LANGUAGE OverloadedStrings #-}
module Lax.Web where

import qualified Data.Text.Lazy as T

import Control.Monad.Reader (runReaderT, asks)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT)

import Database.Persist.Postgresql (withPostgresqlPool, runSqlPool, SqlPersistT)
import qualified Database.Persist as P
import Database.Persist.Sql (ConnectionPool)

import Web.Scotty.Trans

import Lax.Config
import Lax.DB (connString, initDB)
import Lax.DB.Models (Key(..))


app :: (MonadIO m) => ScottyT T.Text (ConfigM m) ()
app =
  get "/" $ do
    user <- runDB $ P.get (UserKey 1)
    html $ "hello, " `T.append` T.pack (show user)

runDB :: (Monad m, MonadTrans t, MonadIO (t (ConfigM m))) => SqlPersistT IO a -> t (ConfigM m) a
runDB query = do
  dbPool <- lift $ asks pool
  liftIO $ runSqlPool query dbPool

runAppWithDbPool :: MonadIO m => ConnectionPool -> m ()
runAppWithDbPool p = do
    fileConfig <- liftIO $ readConfig "./lax.yaml"
    let cfg = AppConfig p fileConfig
        r m = runReaderT (runConfigM m) cfg
    -- liftIO $ print fconf
    scottyT 25000 r app

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connString 10 $ \p -> do
  liftIO $ runSqlPool initDB p
  runAppWithDbPool p
