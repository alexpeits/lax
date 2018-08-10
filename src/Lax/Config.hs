{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lax.Config where

import Control.Monad.Reader

import Database.Persist.Sql (ConnectionPool)


data Config = Config
  { pool :: ConnectionPool
  }

newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a
  } deriving (Applicative, Functor, Monad,
             MonadIO, MonadReader Config)
