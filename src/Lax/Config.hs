{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lax.Config
  (
    Config(..)
  , readConfig
  ) where

import Control.Monad.Reader

import Database.Persist.Sql (ConnectionPool)

import qualified Lax.Config.Types as AppConfig
import Lax.Config.File (readConfig)


data Config = MkConfig
  { pool :: ConnectionPool
  , conf :: AppConfig.Config
  }
