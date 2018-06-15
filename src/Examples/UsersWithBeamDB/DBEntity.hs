{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Examples.UsersWithBeamDB.DBEntity where

import Data.Text (Text)
import Data.Time
import GHC.Generics (Generic)

import Database.Beam
import Database.Beam.Backend.SQL.Types (unSerial)
import Database.Beam.Postgres
import qualified Database.PostgreSQL.Simple as Pg

import DBEntity
import qualified Examples.UsersWithBeamDB.Database as DB
import Examples.UsersWithBeamDB.Model
import Examples.UsersWithBeamDB.RunDB
import Examples.UsersWithBeamDB.ServerConfig
import Model

getUsers :: Pg [DB.User]
getUsers = runSelectReturningList . select . all_ $ (DB._user DB.demoBeamRestDb)

instance DBEntity DB.User where
  type MonadDB DB.User = ServerConfigReader
  getAllEntities _ = do
    users <- runDB getUsers
    pure users
  save user = pure undefined
  deleteFromDB _ _ = pure undefined

instance DBConvertable User DB.User where
  type DBModel User = DB.User
  type Relations User = Auth
  dbConvertTo user rels = undefined
  dbConvertFrom DB.User {..} _ =
    User
      (Id $ unSerial _userId)
      _userFirstName
      _userLastName
      _userCreatedAt
      _userIsStaff
