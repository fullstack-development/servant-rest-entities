{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module RestEntities.Examples.UsersWithBeamDB.DataSource where

import Data.Typeable
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax

import RestEntities.DataProvider
import qualified RestEntities.Examples.UsersWithBeamDB.Database as DB
import RestEntities.Examples.UsersWithBeamDB.GenericBeam
import RestEntities.Examples.UsersWithBeamDB.Model
import RestEntities.Examples.UsersWithBeamDB.ServerConfig
import RestEntities.Model

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
  auth <- all_ (DB._auth DB.demoBeamRestDb)
  user <- innerJoinRelation auth
  pure (user, auth)

saveUserFromModel :: User -> ServerConfigReader (DB.User, DB.Auth)
saveUserFromModel userModel = do
  Just user <-
    createEntity
      (BeamCreateStructure
         (DB.User
            default_
            (val_ $ userFirstName userModel)
            (val_ $ userLastName userModel)
            (val_ $ userCreatedAt userModel)
            (val_ $ userIsStaff userModel)))
  Just auth <-
    createEntity
      (BeamCreateStructure
         (DB.Auth
            default_
            (val_ $ authPassword $ userAuth userModel)
            (val_ $ authCreatedAt $ userAuth userModel)
            (val_ $ pk user)))
  pure (user, auth)

deleteUserFromDB :: Int -> Pg (Either String ())
deleteUserFromDB entityId =
  runDelete
    (delete (DB._user DB.demoBeamRestDb) (\u -> DB._userId u ==. val_ entityId)) >>
  pure (Right ())

instance HasRelation DB.User DB.Auth where
  getPkSelector _ _ auth =
    let DB.UserId pk = DB._authUserId auth
     in pk

instance HasDataProvider User where
  type DataProviderModel User = DB.User
  type MonadDataProvider User = ServerConfigReader
  type ChildRelations User = SingleChild Auth
  type ParentRelations User = ()
  getPK _ = DB._userId
  filter _ = pure []
  loadAll _ =
    map (\(user, auth) -> unpack user (auth, ())) <$> runDS selectUsersWithAuth
  save user = do
    (savedUser, savedAuth) <- saveUserFromModel user
    return $ unpack savedUser (savedAuth, ())
  deleteById _ = runDS . deleteUserFromDB
  loadById _ pk = do
    result <- runDS . getUserByIdWithAuth $ pk
    let entity =
          maybe Nothing (\(user, auth) -> Just $ unpack user (auth, ())) result
    return entity
  pack user@User {..} _ = (dpUser, dpAuth)
    where
      dpUser =
        DB.User
          (fromId userId)
          userFirstName
          userLastName
          userCreatedAt
          userIsStaff
      dpAuth = pack userAuth user
  unpack DB.User {..} relations =
    User
      (Id _userId)
      _userFirstName
      _userLastName
      _userCreatedAt
      _userIsStaff
      (uncurry unpack relations)

type instance FilterFieldValue Auth "id" = Int

instance DataProvider Identity where
  type DataProviderTypeClass Identity = Eq
  type CreateDataStructure Identity = Identity

instance HasRelation () ()

instance HasDataProvider EmptyChild where
  type DataProviderModel EmptyChild = ()
  type MonadDataProvider EmptyChild = Identity
  type ChildRelations EmptyChild = EmptyChild
  type ParentRelations EmptyChild = EmptyChild

instance HasRelation DB.Auth () where
  getPkSelector _ _ _ = 0

instance HasDataProvider Auth where
  type DataProviderModel Auth = DB.Auth
  type MonadDataProvider Auth = ServerConfigReader
  type ChildRelations Auth = EmptyChild
  type ParentRelations Auth = User
  save user = pure undefined
  deleteById _ _ = pure undefined
  loadById _ _ = pure Nothing
  getRelated = undefined
  pack user rels = undefined
  unpack DB.Auth {..} _ = Auth (Id _authId) _authPassword _authCreatedAt
  filter [filtering] = do
    let q =
          case cast filtering of
            Just (ByEqField _ value :: Filter Auth "id") ->
              Just $ queryById value
            Nothing -> Nothing
    case q of
      Just query -> do
        entities <-
          runDS (runSelectReturningList $ select query :: Pg [DB.Auth])
        relations <- mapM (getRelated (Proxy :: Proxy Auth)) entities
        let denormalized = zip entities relations
        let models = map (uncurry unpack) denormalized :: [Auth]
        return models
      Nothing -> return []
    where
      queryById ::
           Int
        -> Q PgSelectSyntax DB.DemoBeamRestDb s (DB.AuthT (QExpr PgExpressionSyntax s))
      queryById pk =
        filter_ (\a -> DB._authId a ==. val_ pk) $
        all_ (DB._auth DB.demoBeamRestDb)
