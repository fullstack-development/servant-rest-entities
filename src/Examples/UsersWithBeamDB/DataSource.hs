{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Examples.UsersWithBeamDB.DataSource where

import qualified Data.List as L
import Database.Beam
import qualified Database.Beam.Backend.SQL.BeamExtensions as BeamExtensions
import Database.Beam.Backend.SQL.Types (unSerial)
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax

import DataProvider
import qualified Examples.UsersWithBeamDB.Database as DB
import Examples.UsersWithBeamDB.Model
import Examples.UsersWithBeamDB.ServerConfig
import Model

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

type instance ModelOfDataProvider DB.User = User

instance HasDataProvider User where
  type DataProviderModel User = DB.User
  type MonadDataProvider User = ServerConfigReader
  type ChildRelations User = Auth
  type ParentRelations User = ()
  loadAll _ =
    map (\(user, auth) -> unpack user (Just auth)) <$> runDS selectUsersWithAuth
  save user = do
    (savedUser, savedAuth) <- runDS . saveUserFromModel $ user
    return $ unpack savedUser (Just savedAuth)
  deleteById _ = runDS . deleteUserFromDB
  loadById _ pk = do
    result <- runDS . getUserByIdWithAuth $ pk
    let entity =
          maybe Nothing (\(user, auth) -> Just $ unpack user (Just auth)) result
    return entity
  pack User {..} _ =
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
  unpack DB.User {..} (Just auth) =
    User
      (Id _userId)
      _userFirstName
      _userLastName
      _userCreatedAt
      _userIsStaff
      (unpack auth Nothing)

type instance ModelOfDataProvider DB.Auth = Auth

instance HasDataProvider Auth where
  type DataProviderModel Auth = DB.Auth
  type MonadDataProvider Auth = ServerConfigReader
  type ChildRelations Auth = ()
  type ParentRelations Auth = User
  loadAll _ = undefined
  save user = pure undefined
  deleteById _ _ = pure undefined
  loadById _ _ = pure Nothing
  pack user rels = undefined
  unpack DB.Auth {..} _ = Auth (Id _authId) _authPassword _authCreatedAt
