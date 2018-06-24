{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module DataProvider where

import Data.Kind
import Data.Proxy

type family MapDataProviders e where
  MapDataProviders () = ()
  MapDataProviders (a, b) = (MapDataProviders a, MapDataProviders b)
  MapDataProviders (a, b, c) = ( MapDataProviders a
                               , MapDataProviders b
                               , MapDataProviders c)
  MapDataProviders (a, b, c, d) = ( MapDataProviders a
                                  , MapDataProviders b
                                  , MapDataProviders c
                                  , MapDataProviders d)
  MapDataProviders (a, b, c, d, e) = ( MapDataProviders a
                                     , MapDataProviders b
                                     , MapDataProviders c
                                     , MapDataProviders d
                                     , MapDataProviders e)
  MapDataProviders (a, b, c, d, e, h) = ( MapDataProviders a
                                        , MapDataProviders b
                                        , MapDataProviders c
                                        , MapDataProviders d
                                        , MapDataProviders e
                                        , MapDataProviders h)
  MapDataProviders (a, b, c, d, e, h, g) = ( MapDataProviders a
                                           , MapDataProviders b
                                           , MapDataProviders c
                                           , MapDataProviders d
                                           , MapDataProviders e
                                           , MapDataProviders h
                                           , MapDataProviders g)
  MapDataProviders e = DataProviderModel e

type family ModelOfDataProvider dbmodel :: *

type LoadAllConstraint model
   = ( Monad (MonadDataProvider model)
     , DataProviderTypeClass (MonadDataProvider model) (DataProviderModel model)
     , DataProvider (MonadDataProvider model))

class (ModelOfDataProvider (DataProviderModel model) ~ model) =>
      HasDataProvider model
  where
  type DataProviderModel model
  type MonadDataProvider model :: * -> *
  type ChildRelations model
  type ParentRelations model
  unpack ::
       DataProviderModel model
    -> Maybe (MapDataProviders (ChildRelations model))
    -> model
  pack ::
       model
    -> Maybe (ParentRelations model)
    -> (DataProviderModel model, MapDataProviders (ChildRelations model))
  save :: model -> MonadDataProvider model model
  loadById :: Proxy model -> Int -> MonadDataProvider model (Maybe model)
  loadAll :: Proxy model -> MonadDataProvider model [model]
  default loadAll :: LoadAllConstraint model =>
    Proxy model -> MonadDataProvider model [model]
  -- TODO load relations for each model
  loadAll proxyModel =
    map (`unpack` undefined) <$>
    getAllEntities (Proxy :: Proxy (DataProviderModel model))
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
