{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Lax.DB.Models where

import Data.ByteString (ByteString)

import Database.Persist.TH

import Lax.DB.Types (Email)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
    username String
    UniqueUserUsername username
    email Email
    UniqueUserEmail email
    password ByteString
    deriving Show

Organization sql=organizations
    name String
    deriving Show

UserOrganization sql=user_organizations
    userId UserId
    organizationId OrganizationId
    UniqueUserOrganization userId organizationId
|]
