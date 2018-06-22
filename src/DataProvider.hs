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
{-# LANGUAGE GADTs #-}

module DataProvider where

import Data.Proxy

type family DataProviderModel e

type instance DataProviderModel () = ()

type instance DataProviderModel (a, b) =
     (DataProviderModel a, DataProviderModel b)

type instance DataProviderModel (a, b, c) =
     (DataProviderModel a, DataProviderModel b, DataProviderModel c)

type instance DataProviderModel (a, b, c, d) =
     (DataProviderModel a, DataProviderModel b, DataProviderModel c,
      DataProviderModel d)

type instance DataProviderModel (a, b, c, d, e) =
     (DataProviderModel a, DataProviderModel b, DataProviderModel c,
      DataProviderModel d, DataProviderModel e)

type instance DataProviderModel (a, b, c, d, e, f) =
     (DataProviderModel a, DataProviderModel b, DataProviderModel c,
      DataProviderModel d, DataProviderModel e, DataProviderModel f)

type instance DataProviderModel (a, b, c, d, e, f, h) =
     (DataProviderModel a, DataProviderModel b, DataProviderModel c,
      DataProviderModel d, DataProviderModel e, DataProviderModel f,
      DataProviderModel h)

class HasDataProvider model dsmodel | model -> dsmodel where
  type MonadDataProvider model :: * -> *
  type ChildRelations model
  type ParentRelations model
  unpack :: dsmodel -> Maybe (DataProviderModel (ChildRelations model)) -> model
  pack ::
       model
    -> Maybe (ParentRelations model)
    -> (dsmodel, DataProviderModel (ChildRelations model))
  save :: model -> MonadDataProvider model model
  loadById :: Proxy model -> Int -> MonadDataProvider model (Maybe model)
  loadAll :: Proxy model -> MonadDataProvider model [model]
  deleteById :: Proxy model -> Int -> MonadDataProvider model (Either String ())

class HasDataSourceRun (actionMonad :: * -> *) (dsMonad :: * -> *) where
  runDS :: dsMonad a -> actionMonad a
