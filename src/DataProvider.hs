{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module DataProvider where

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

class HasDataProvider model where
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
  deleteById :: Proxy model -> Int -> MonadDataProvider model (Either String ())

class HasDataSourceRun (actionMonad :: * -> *) (dsMonad :: * -> *) where
  runDS :: dsMonad a -> actionMonad a
