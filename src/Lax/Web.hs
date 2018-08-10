{-# LANGUAGE OverloadedStrings #-}
module Lax.Web where

import qualified Data.Text.Lazy as T

-- import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT)
import Control.Monad.Identity

import Web.Scotty.Trans
import Database.Persist.Postgresql (Key(..), withPostgresqlPool, runSqlPool, SqlPersistT)
import qualified Database.Persist as P
import Database.Persist.Sql (ConnectionPool)

import Lax.Config
import Lax.DB (connString, initDB)
import Lax.DB.Models (User(..), Key(..))


app :: ScottyT T.Text ConfigM ()
app =
  get "/" $ do
    -- uid <- liftIO $ runDB $ P.insert $ User "Alex" "alex@foo.com" "pass"
    alex <- runDB $ P.get (UserKey 1)
    html $ "hello, " `T.append` T.pack (show alex)

runDB :: (MonadTrans t, MonadIO (t ConfigM)) => SqlPersistT IO a -> t ConfigM a
runDB query = do
  dbPool <- lift $ asks pool
  liftIO $ runSqlPool query dbPool

runAppWithDbPool :: MonadIO m => ConnectionPool -> m ()
runAppWithDbPool p = do
    let cfg = Config p
        r m = runReaderT (runConfigM m) cfg
    r $ runIdentityT _
    scottyT 25000 r app

main :: IO ()
-- main = runStderrLoggingT $ withPostgresqlPool connString 10 runAppWithDbPool
main = runStderrLoggingT $ withPostgresqlPool connString 10 $ \p -> do
  liftIO $ runSqlPool initDB p
  runAppWithDbPool p
