{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module DataProvider where

import Data.Kind
import Data.Proxy

type family PackingResult model where
  PackingResult () = ()
  PackingResult [model] = [PackingResult model]
  PackingResult model = ( DataProviderModel model
                        , PackingResult (ChildRelations model))

type LoadAllConstraint model
   = ( Monad (MonadDataProvider model)
     , DataProviderTypeClass (MonadDataProvider model) (DataProviderModel model)
     , DataProvider (MonadDataProvider model))

class HasDataProvider model where
  type DataProviderModel model
  type MonadDataProvider model :: * -> *
  type ChildRelations model
  type ParentRelations model
  unpack ::
       DataProviderModel model -> PackingResult (ChildRelations model) -> model
  pack :: model -> ParentRelations model -> PackingResult model
  save :: model -> MonadDataProvider model model
  --
  --
  loadById :: Proxy model -> Int -> MonadDataProvider model (Maybe model)
  default loadById :: LoadAllConstraint model =>
    Proxy model -> Int -> MonadDataProvider model (Maybe model)
  loadById _ pk = do
    entity <- getEntityById (Proxy :: Proxy (DataProviderModel model)) pk
    pure $ (`unpack` undefined) <$> entity
  --
  --
  loadAll :: Proxy model -> MonadDataProvider model [model]
  default loadAll :: LoadAllConstraint model =>
    Proxy model -> MonadDataProvider model [model]
  loadAll proxyModel = do
    entities <- getAllEntities (Proxy :: Proxy (DataProviderModel model))
    pure $ (`unpack` undefined) <$> entities
  --
  --
  deleteById :: Proxy model -> Int -> MonadDataProvider model (Either String ())

class HasDataSourceRun (actionMonad :: * -> *) (dsMonad :: * -> *) where
  runDS :: dsMonad a -> actionMonad a

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
