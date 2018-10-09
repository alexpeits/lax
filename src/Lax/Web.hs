{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lax.Web where

import qualified Data.Text.Lazy as T
import qualified Data.ByteString as BS

import Control.Monad.Reader
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT)


import Database.Persist.Postgresql (withPostgresqlPool, runSqlPool, SqlPersistT)
import qualified Database.Persist as P
import Database.Persist.Sql (ConnectionPool)

import Web.Scotty.Trans
import qualified Web.Scotty as S

import Network.Wai (Application)

import Test.Hspec
import qualified Test.Hspec.Wai as TW

import qualified Network.HTTP.Types.Status as HTTPStatus

import Lax.Config
import Lax.DB (connString, initDB, hashPassword)
import Lax.DB.Types (Email)
import Lax.DB.Models (Key(..), UserId, User(..))

import Control.Exception
import Data.Aeson hiding (json)
import GHC.Generics

data UserRegisterRequest = UserRegisterRequest
  { username :: String
  , email :: Email
  , password :: T.Text
  } deriving (Show, Generic)

instance FromJSON UserRegisterRequest

fromJSONRequest :: (FromJSON a, MonadIO m)
                => (T.Text -> ActionT T.Text m a) -> ActionT T.Text m a
fromJSONRequest onErr = jsonData `rescue` onErr

data AppErr = MkAppErr
  { description :: T.Text
  , errors :: [T.Text]
  } deriving (Show, Generic)

instance ToJSON AppErr

httpErr :: MonadIO m => HTTPStatus.Status -> T.Text -> [T.Text] -> ActionT T.Text m a
httpErr st desc err = do
  let errInfo = MkAppErr desc err
  json errInfo
  status st
  finish

errInvalidInputData :: MonadIO m => T.Text -> ActionT T.Text m a
errInvalidInputData = httpErr HTTPStatus.badRequest400 "InvalidInputData" . pure

errHashingPassword :: MonadIO m => ActionT T.Text m a
errHashingPassword = httpErr HTTPStatus.internalServerError500 "Error hashing password" []


app :: (MonadIO m, MonadDB m) => ScottyT T.Text m ()
app = do
  -- defaultHandler $ \x -> do
    -- status HTTPStatus.notFound404
    -- text x
  get "/test" $ do
    liftIO $ putStrLn "/test"
    liftAndCatchIO $ throw DivideByZero
    liftIO $ putStrLn "run?"
  get "/" $ do
    -- user <- lift $ runDB $ P.get (UserKey 1)
    user <- getUser (UserKey 1)
    html $ "hello, " `T.append` T.pack (show user)
  post "/register" $ do
    UserRegisterRequest un em pw <- jsonData `rescue` errInvalidInputData
    hashRes <- liftIO $ hashPassword pw
    case hashRes of
      Nothing -> errHashingPassword
      Just hashedPw -> do
        uid <- addUser $ User un em hashedPw
        html $ T.pack (show uid)
  -- notFound $ do
  --   liftIO $ putStrLn "not found"
  --   finish

-- runDB :: (MonadTrans t, MonadIO (t AppM)) => SqlPersistT IO a -> t AppM a
-- runDB query = do
  -- dbPool <- lift $ asks pool
  -- liftIO $ runSqlPool query dbPool

runDB :: (MonadIO m, MonadReader Config m) => SqlPersistT IO a -> m a
runDB query = do
  dbPool <- asks pool
  liftIO $ runSqlPool query dbPool

runAppWithDbPool :: MonadIO m => ConnectionPool -> m ()
runAppWithDbPool p = do
    fileConfig <- liftIO $ readConfig "./lax.yaml"
    let cfg = MkConfig p fileConfig
        r m = runReaderT (unAppM m) cfg
    -- liftIO $ print fconf
    scottyT 25000 r app

main :: IO ()
-- main = runStderrLoggingT $ withPostgresqlPool connString 10 runAppWithDbPool
main = runStderrLoggingT $ withPostgresqlPool connString 10 $ \p -> do
  liftIO $ runSqlPool initDB p
  runAppWithDbPool p

newtype AppM a = MkAppM { unAppM :: ReaderT Config IO a}
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Config
           )

class Monad m => MonadDB m where
  getUser :: UserId -> m (Maybe User)
  addUser :: User -> m UserId

-- instance MonadIO m => MonadDB (SqlPersistT m) where
--   getUser = P.get

instance MonadDB AppM where
  getUser = runDB . P.get
  addUser = runDB . P.insert

instance (MonadDB m, ScottyError e) => MonadDB (ActionT e m) where
  getUser = lift . getUser
  addUser = lift . addUser

----
-- Testing
----


testApp :: (MonadDB m, MonadIO m, MonadReader Int m) => ScottyT T.Text m ()
testApp = do
  get "/" $ do
    a <- (+1) <$> lift ask
    (Just u) <- lift $ getUser (UserKey 1)
    text $ T.append (T.pack $ userUsername u) (T.pack (show a))

newtype TestM a = MkTestM { unTestM :: ReaderT Int IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Int
           )

instance MonadDB TestM where
  getUser uid = return $ Just $ User "Alex" "asfd@asfd.com" "fsa"
  addUser = undefined

runTestApp :: IO Application
runTestApp = do
  let r m = runReaderT (unTestM m) 0
  scottyAppT r testApp

spec :: Spec
spec = TW.with runTestApp $ do
  describe "testing" $ do
    it "tests ok" $ do
      TW.get "/" `TW.shouldRespondWith` "Alex1"
