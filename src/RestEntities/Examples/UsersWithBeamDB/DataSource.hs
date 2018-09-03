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
import Prelude hiding (filter)

import RestEntities.DataProvider
import qualified RestEntities.Examples.UsersWithBeamDB.Database as DB
import RestEntities.Examples.UsersWithBeamDB.GenericBeam
import RestEntities.Examples.UsersWithBeamDB.Model
import RestEntities.Examples.UsersWithBeamDB.ServerConfig
import RestEntities.HasDataProvider
import RestEntities.HasDataSourceRun
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
  user <- all_ (DB._user DB.demoBeamRestDb)
  auth <- innerJoinRelation user
  pure (user, auth)

saveUserFromModel :: User -> ServerConfigReader (DB.User, DB.Auth)
saveUserFromModel userModel = do
  Just auth <-
    createEntity
      (BeamCreateStructure
         (DB.Auth
            default_
            (val_ $ authPassword $ userAuth userModel)
            (val_ $ authCreatedAt $ userAuth userModel)))
  Just user <-
    createEntity
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

instance HasDataProvider User where
  type DataProviderModel User = DB.User
  type MonadDataProvider User = ServerConfigReader
  type ChildRelations User = SingleChild Auth
  type ParentRelations User = ()
  pack user@User {..} _ = (dpUser, dpAuth)
    where
      dpUser =
        DB.User
          (fromId userId)
          userFirstName
          userLastName
          userCreatedAt
          userIsStaff
          (DB.AuthId $ fromId $ authId userAuth)
      dpAuth = pack userAuth (user, ())
  unpack DB.User {..} relations =
    User
      (Id _userId)
      _userFirstName
      _userLastName
      _userCreatedAt
      _userIsStaff
      (uncurry unpack relations)

instance HasDataProviderSaveable User where
  save user _ = do
    (savedUser, savedAuth) <- saveUserFromModel user
    return $ unpack savedUser (savedAuth, ())

instance HasDataProviderLoadable User where
  loadAll _ =
    map (\(user, auth) -> unpack user (auth, ())) <$> runDS selectUsersWithAuth
  loadById _ pk = do
    result <- runDS . getUserByIdWithAuth $ pk
    let entity =
          maybe Nothing (\(user, auth) -> Just $ unpack user (auth, ())) result
    return entity

instance HasDataProviderDeleteable User where
  deleteById _ = runDS . deleteUserFromDB

type instance FilterFieldValue Auth "id" = Int

instance HasDataProvider Auth where
  type DataProviderModel Auth = DB.Auth
  type MonadDataProvider Auth = ServerConfigReader
  type ChildRelations Auth = EmptyChild
  type ParentRelations Auth = User
  pack user rels = undefined
  unpack DB.Auth {..} _ = Auth (Id _authId) _authPassword _authCreatedAt

instance HasDataProviderSaveable Auth where
  save user = pure undefined

instance HasDataProviderFilterable Auth where
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
        relations <- mapM (loadChildRelations (Proxy :: Proxy Auth)) entities
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

instance HasDataProviderLoadable Auth where
  loadById _ _ = pure Nothing
  loadChildRelations = undefined

instance HasDataProviderDeleteable Auth where
  deleteById _ _ = pure undefined
