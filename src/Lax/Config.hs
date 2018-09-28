{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lax.Config
  (
    AppConfig(..)
  , ConfigM (runConfigM)
  , readConfig
  ) where

import Control.Monad.Reader

import Database.Persist.Sql (ConnectionPool)

import Lax.Config.Types (Config)
import Lax.Config.File (readConfig)


data AppConfig = AppConfig
  { pool :: ConnectionPool
  , conf :: Config
  }

newtype ConfigM m a = ConfigM
  { runConfigM :: ReaderT AppConfig m a
  } deriving (Applicative, Functor, Monad,
             MonadIO, MonadReader AppConfig)
