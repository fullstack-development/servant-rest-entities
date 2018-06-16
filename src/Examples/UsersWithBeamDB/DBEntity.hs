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
getUsers = runSelectReturningList . select . all_ . DB._user $ DB.demoBeamRestDb

selectUsersWithAuth :: Pg [(DB.User, DB.Auth)]
selectUsersWithAuth =
  runSelectReturningList $
  select $ do
    user <- all_ (DB._user DB.demoBeamRestDb)
    auth <- all_ (DB._auth DB.demoBeamRestDb)
    guard_ (DB._userAuthId user `references_` auth)
    pure (user, auth)

instance DBEntity User DB.User where
  type MonadDB DB.User = ServerConfigReader
  getAllFromDB = runDB getUsers
  save user = pure undefined
  deleteFromDB _ _ = pure undefined
  getByIdFromDB _ = pure Nothing
  getByIdWithRelsFromDB _ _ = undefined
  getAllFromDBWithRels = runDB selectUsersWithAuth

type instance DBModel User = DB.User

instance DBConvertable User DB.User where
  type ChildRelations User = Auth
  type ParentRelations User = ()
  dbConvertTo user rels = undefined
  dbConvertFrom DB.User {..} (Just auth) =
    User
      (Id $ unSerial _userId)
      _userFirstName
      _userLastName
      _userCreatedAt
      _userIsStaff
      (dbConvertFrom auth Nothing)

type instance DBModel Auth = DB.Auth

instance DBEntity Auth DB.Auth where
  type MonadDB DB.Auth = ServerConfigReader
  getAllFromDB = undefined
  save user = pure undefined
  deleteFromDB _ _ = pure undefined
  getByIdFromDB _ = pure Nothing
  getByIdWithRelsFromDB _ _ = undefined
  getAllFromDBWithRels = undefined

instance DBConvertable Auth DB.Auth where
  type ChildRelations Auth = ()
  type ParentRelations Auth = User
  dbConvertTo user rels = undefined
  dbConvertFrom DB.Auth {..} _ =
    Auth (Id $ unSerial _authId) _authPassword _authCreatedAt
