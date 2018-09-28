{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Lax.Config.File where

import Data.ByteString
import System.FilePath

import Text.RawString.QQ
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON, (.:), (.:?))

import Lax.Config.Types

configYaml :: ByteString
configYaml = [r|
POSTGRES:
  # host: "localhost"
  port: 5432
  db: "lax"
  user: "postgres"
  password: "pass"

WEB_SERVER:
  host: "localhost"
  port: 25000
|]


instance FromJSON PostgresConfig where
  parseJSON (Y.Object v) =
    PostgresConfig <$>
    v .:? "host" <*>
    v .:? "port" <*>
    v .:  "db" <*>
    v .:  "user" <*>
    v .:  "password"
  parseJSON _ = fail "Error parsing postgres config"

instance FromJSON WebServerConfig where
  parseJSON (Y.Object v) =
    WebServerConfig <$>
    v .:? "host" <*>
    v .:  "port"
  parseJSON _ = fail "Error parsing webserver config"

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
    v .: "POSTGRES" <*>
    v .: "WEB_SERVER"
  parseJSON _ = fail "Error parsing config"

readConfig :: FilePath -> IO Config
readConfig = Y.decodeFileThrow
