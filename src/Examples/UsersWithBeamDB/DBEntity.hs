{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Examples.UsersWithBeamDB.DBEntity where

import Database.Beam
import qualified Database.Beam.Backend.SQL.BeamExtensions as BeamExtensions
import Database.Beam.Backend.SQL.Types (unSerial)
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax

import DBEntity
import qualified Examples.UsersWithBeamDB.Database as DB
import Examples.UsersWithBeamDB.Model
import Examples.UsersWithBeamDB.ServerConfig
import Model

getUsers :: Pg [DB.User]
getUsers = runSelectReturningList . select . all_ . DB._user $ DB.demoBeamRestDb

getUserById :: Int -> Pg (Maybe DB.User)
getUserById idvalue =
  runSelectReturningOne $
  select $
  filter_
    (\u -> pk u ==. val_ (DB.UserId idvalue))
    (all_ $ DB._user DB.demoBeamRestDb)

getUserByIdWithAuth :: Int -> Pg (Maybe (DB.User, DB.Auth))
getUserByIdWithAuth idvalue =
  runSelectReturningOne $
  select $
  filter_ (\(u, _) -> pk u ==. val_ (DB.UserId idvalue)) queryUserWithAuth

selectUsersWithAuth :: Pg [(DB.User, DB.Auth)]
selectUsersWithAuth = runSelectReturningList $ select queryUserWithAuth

queryUserWithAuth ::
     Q PgSelectSyntax DB.DemoBeamRestDb s ( DB.UserT (QExpr PgExpressionSyntax s)
                                          , DB.AuthT (QExpr PgExpressionSyntax s))
queryUserWithAuth = do
  user <- all_ (DB._user DB.demoBeamRestDb)
  auth <- all_ (DB._auth DB.demoBeamRestDb)
  guard_ (DB._userAuthId user `references_` auth)
  pure (user, auth)

saveUser :: DB.User -> Pg DB.User
saveUser user =
  head <$>
  BeamExtensions.runInsertReturningList
    (DB._user DB.demoBeamRestDb)
    (insertValues [user])

saveUserFromModel :: User -> Pg (DB.User, DB.Auth)
saveUserFromModel userModel = do
  [auth] <-
    BeamExtensions.runInsertReturningList
      (DB._auth DB.demoBeamRestDb)
      (insertExpressions
         [ DB.Auth
             default_
             (val_ $ authPassword $ userAuth userModel)
             (val_ $ authCreatedAt $ userAuth userModel)
         ])
  [user] <-
    BeamExtensions.runInsertReturningList
      (DB._user DB.demoBeamRestDb)
      (insertExpressions
         [ DB.User
             default_
             (val_ $ userFirstName userModel)
             (val_ $ userLastName userModel)
             (val_ $ userCreatedAt userModel)
             (val_ $ userIsStaff userModel)
             (val_ $ pk auth)
         ])
  pure (user, auth)

deleteUserFromDB :: Int -> Pg (Either String ())
deleteUserFromDB entityId =
  runDelete
    (delete (DB._user DB.demoBeamRestDb) (\u -> DB._userId u ==. val_ entityId)) >>
  pure (Right ())

instance DBEntity User DB.User where
  type MonadDB DB.User = ServerConfigReader
  type ChildRelations User = Auth
  type ParentRelations User = ()
  getAllFromDB = runDB getUsers
  save = runDB . saveUser
  deleteFromDB _ = runDB . deleteUserFromDB
  getByIdFromDB = runDB . getUserById
  getByIdWithRelsFromDB _ = runDB . getUserByIdWithAuth
  getAllFromDBWithRels _ = runDB selectUsersWithAuth

type instance DBModel User = DB.User

instance DBConvertable User DB.User where
  dbConvertTo User {..} _ =
    ( DB.User
        (fromId userId)
        userFirstName
        userLastName
        userCreatedAt
        userIsStaff
        (DB.AuthId $ fromId $ authId userAuth)
    , DB.Auth
        (fromId $ authId userAuth)
        (authPassword userAuth)
        (authCreatedAt userAuth))
  dbConvertFrom DB.User {..} (Just auth) =
    User
      (Id _userId)
      _userFirstName
      _userLastName
      _userCreatedAt
      _userIsStaff
      (dbConvertFrom auth Nothing)

type instance DBModel Auth = DB.Auth

instance DBEntity Auth DB.Auth where
  type MonadDB DB.Auth = ServerConfigReader
  type ChildRelations Auth = ()
  type ParentRelations Auth = User
  getAllFromDB = undefined
  save user = pure undefined
  deleteFromDB _ _ = pure undefined
  getByIdFromDB _ = pure Nothing
  getByIdWithRelsFromDB _ _ = undefined
  getAllFromDBWithRels = undefined

instance DBConvertable Auth DB.Auth where
  dbConvertTo user rels = undefined
  dbConvertFrom DB.Auth {..} _ = Auth (Id _authId) _authPassword _authCreatedAt
