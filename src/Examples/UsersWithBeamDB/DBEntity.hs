{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
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
getUsers = runSelectReturningList . select . all_ $ DB._user DB.demoBeamRestDb

instance DBEntity User DB.User where
  type MonadDB DB.User = ServerConfigReader
  type ChildRelations User = Auth
  type ParentRelations User = ()
  getAllFromDB = runDB getUsers
  save user = pure undefined
  deleteFromDB _ _ = pure undefined
  getByIdFromDB _ = pure Nothing
  getByIdWithRelsFromDB _ _ = undefined
  getAllFromDBWithRels = undefined

type instance DBModel User = DB.User

instance DBConvertable User DB.User where
  dbConvertTo user rels = undefined
  dbConvertFrom DB.User {..} _ =
    User
      (Id $ unSerial _userId)
      _userFirstName
      _userLastName
      _userCreatedAt
      _userIsStaff
