{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
import qualified Examples.UsersWithBeamDB.Model as Model
import Examples.UsersWithBeamDB.Resources
import Examples.UsersWithBeamDB.RunDB
import Examples.UsersWithBeamDB.ServerConfig
import Model
import Resource

getUsers :: Pg [DB.User]
getUsers = runSelectReturningList . select . all_ . DB._user $ DB.demoBeamRestDb

selectUsersWithAuth :: Pg [(DB.User, DB.Auth)]
selectUsersWithAuth =
  runSelectReturningList $
  select $ do
    user <- all_ (DB._user DB.demoBeamRestDb)
    auth <- all_ (DB._auth DB.demoBeamRestDb)
    guard_ (DB._userAuthId user `references_` auth)
    pure (user, auth)

instance DBEntity Model.User DB.User where
  getAllFromDB = undefined -- runDB getUsers
  save user = pure undefined
  deleteFromDB _ _ = undefined
  getByIdFromDB _ = undefined
  getByIdWithRelsFromDB _ _ = undefined
  getAllFromDBWithRels = undefined -- runDB selectUsersWithAuth

type instance DBModel Model.User = DB.User

instance DBConvertable Model.User DB.User where
  type ChildRelations Model.User = Model.Auth
  type ParentRelations Model.User = ()
  dbConvertTo user rels = undefined
  dbConvertFrom DB.User {..} (Just auth) =
    Model.User
      (Id $ unSerial _userId)
      _userFirstName
      _userLastName
      _userCreatedAt
      _userIsStaff
      (dbConvertFrom auth Nothing)

type instance DBModel Model.Auth = DB.Auth

instance DBEntity Model.Auth DB.Auth where
  getAllFromDB = undefined
  save user = undefined
  deleteFromDB _ _ = undefined
  getByIdFromDB _ = undefined
  getByIdWithRelsFromDB _ _ = undefined
  getAllFromDBWithRels = undefined

instance DBConvertable Model.Auth DB.Auth where
  type ChildRelations Model.Auth = ()
  type ParentRelations Model.Auth = Model.User
  dbConvertTo user rels = undefined
  dbConvertFrom DB.Auth {..} _ =
    Model.Auth (Id $ unSerial _authId) _authPassword _authCreatedAt
