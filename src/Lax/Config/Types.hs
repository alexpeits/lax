module Lax.Config.Types where

import Data.Text(Text)

data PostgresConfig = PostgresConfig
  { postgresHost :: Maybe Text
  , postgresPort :: Maybe Int
  , postgresDb :: Text
  , postgresUser :: Text
  , postgresPassword :: Text
  } deriving (Eq, Show)

data WebServerConfig = WebServerConfig
  { webServerHost :: Maybe Text
  , webServerPort :: Int
  } deriving (Eq, Show)

data Config =
  Config {
    postgres  :: PostgresConfig
  , webServer  :: WebServerConfig
  } deriving (Eq, Show)

data ConfigError = ConfigFileError String deriving Show
