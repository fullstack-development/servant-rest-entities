{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Examples.UsersWithBeamDB.DataSource where

import Data.Proxy
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Examples.UsersWithBeamDB.GenericBeam

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
  auth <- innerJoinRelation user
  pure (user, auth)

saveUserFromModel :: User -> ServerConfigReader (DB.User, DB.Auth)
saveUserFromModel userModel = do
  Just auth <-
    createEntity
      (Proxy :: Proxy DB.Auth)
      (BeamCreateStructure
         (DB.Auth
            default_
            (val_ $ authPassword $ userAuth userModel)
            (val_ $ authCreatedAt $ userAuth userModel)))
  Just user <-
    createEntity
      (Proxy :: Proxy DB.User)
      (BeamCreateStructure
         (DB.User
            default_
            (val_ $ userFirstName userModel)
            (val_ $ userLastName userModel)
            (val_ $ userCreatedAt userModel)
            (val_ $ userIsStaff userModel)
            (val_ $ pk auth)))
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
    (savedUser, savedAuth) <- saveUserFromModel user
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
