{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module DataProvider where

import Data.Kind
import Data.Proxy

type family Denormalized model where
  Denormalized () = ()
  Denormalized (a, b) = (Denormalized a, Denormalized b)
  Denormalized (a, b, c) = (Denormalized a, Denormalized b, Denormalized c)
  Denormalized [model] = [Denormalized model]
  Denormalized model = ( DataProviderModel model
                       , Denormalized (ChildRelations model))

type Loadable model
   = ( Monad (MonadDataProvider model)
     , DataProviderTypeClass (MonadDataProvider model) (DataProviderModel model)
     , DataProvider (MonadDataProvider model))

class HasRelationsProvider a where
  type Result a
  loadRelationsById :: Proxy a -> Int -> Result a

instance (HasRelationsProvider a, HasRelationsProvider b) =>
         HasRelationsProvider (a, b) where
  type Result (a, b) = (Result a, Result b)
  loadRelationsById _ pk =
    ( loadRelationsById (Proxy :: Proxy a) pk
    , loadRelationsById (Proxy :: Proxy b) pk)

class (HasRelationsProvider (ChildRelations model)) =>
      HasDataProvider model
  where
  type DataProviderModel model
  type MonadDataProvider model :: * -> *
  type ChildRelations model
  type ParentRelations model
  unpack ::
       DataProviderModel model -> Denormalized (ChildRelations model) -> model
  pack :: model -> ParentRelations model -> Denormalized model
  save :: model -> MonadDataProvider model model
  --
  --
  loadById :: Proxy model -> Int -> MonadDataProvider model (Maybe model)
  default loadById :: Loadable model =>
    Proxy model -> Int -> MonadDataProvider model (Maybe model)
  loadById _ pk = do
    entity <- getEntityById (Proxy :: Proxy (DataProviderModel model)) pk
    case entity of
      Just e -> do
        relations <- getRelated (Proxy :: Proxy model) e
        return $ (`unpack` relations) <$> entity
      _ -> return Nothing
  --
  --
  loadAll :: Proxy model -> MonadDataProvider model [model]
  default loadAll :: Loadable model =>
    Proxy model -> MonadDataProvider model [model]
  loadAll proxyModel = do
    entities <- getAllEntities (Proxy :: Proxy (DataProviderModel model))
    relations <- mapM (getRelated (Proxy :: Proxy model)) entities
    let denormalized = zip entities relations
    let models = map (uncurry unpack) denormalized
    return models
  --
  --
  deleteById :: Proxy model -> Int -> MonadDataProvider model (Either String ())
  getRelated ::
       Proxy model
    -> DataProviderModel model
    -> MonadDataProvider model (Denormalized (ChildRelations model))
  getRelated dpModel = do
    let rels = loadRelationsById (Proxy :: Proxy (ChildRelations model))
    return undefined

class HasDataSourceRun (actionMonad :: * -> *) (dsMonad :: * -> *) where
  runDS :: dsMonad a -> actionMonad a

-- Need to be implemented by concrete relevant data 
-- source backend (db, http services, memory and etc.)
class DataProvider dp where
  type DataProviderTypeClass dp :: * -> Constraint
  type CreateDataStructure dp :: * -> *
  getAllEntities ::
       (DataProviderTypeClass dp dbmodel) => Proxy dbmodel -> dp [dbmodel]
  getEntityById ::
       (DataProviderTypeClass dp dbmodel)
    => Proxy dbmodel
    -> Int
    -> dp (Maybe dbmodel)
  createEntity ::
       (DataProviderTypeClass dp dbmodel)
    => Proxy dbmodel
    -> CreateDataStructure dp dbmodel
    -> dp (Maybe dbmodel)
