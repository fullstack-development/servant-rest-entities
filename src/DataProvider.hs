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
  getAllEntities ::
       (DataProviderTypeClass dp dbmodel) => Proxy dbmodel -> dp [dbmodel]
-- getAllRelations :: (ReduceRels rels, DataProvider dp) => Proxy rels -> dp [rels]
-- getAllRelations proxyRels = do
--   applyReduced getAllEntities (Proxy :: RedusedRels (Proxy rels))
-- class ReduceRels rels where
--   applyReduced ::
--        (forall a. ReduceRels a =>
--                     a -> RedusedRels a)
--     -> rels
--     -> RedusedRels rels
--   runAction ::
--        Monad m
--     => WrappedInAction m rels
--     -> (RedusedActions (WrappedInAction m rels))
-- type family RedusedRels a where
--   RedusedRels (a, b) = (RedusedRels a, RedusedRels b)
--   RedusedRels [a] = [RedusedRels a]
--   RedusedRels a = a
--   -- RedusedRels (Proxy (a, b)) = (RedusedRels a, RedusedRels b)
--   -- RedusedRels (Proxy [a]) = [a]
-- type family WrappedInAction m rels where
--   WrappedInAction m (a, b) = (m a, m b)
--   WrappedInAction m [a] = [m a]
--   WrappedInAction m a = m a
-- type family RedusedActions m where
--   RedusedActions (m a, m b) = m (a, b)
--   RedusedActions [m a] = m [a]
--   RedusedActions ma = ma
-- instance (ReduceRels a, ReduceRels b) => ReduceRels (a, b) where
--   applyReduced f (a, b) = (applyReduced f a, applyReduced f b)
--   runAction (ma, mb) = do
--     a <- ma
--     b <- mb
--     pure (a, b)
-- instance (ReduceRels a) => ReduceRels [a] where
--   applyReduced f = map (applyReduced f)
--   runAction = sequence
-- instance (HasDataProvider a) =>
--          ReduceRels a where
--   applyReduced f = f
--   runAction ma = undefined
-- type family MapProxyToRels a where
--   MapProxyToRels (Proxy (a, b)) = (Proxy a, Proxy b)
--   MapProxyToRels (Proxy [a]) = [Proxy a]
--   MapProxyToRels (Proxy a) = Proxy a
