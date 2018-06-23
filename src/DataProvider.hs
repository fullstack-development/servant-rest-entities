{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module DataProvider where

import Data.Proxy

type family PackingResult model where
  PackingResult () = ()
  PackingResult [model] = [PackingResult model]
  PackingResult model = ( DataProviderModel model
                        , PackingResult (ChildRelations model))

class HasDataProvider model where
  type DataProviderModel model
  type MonadDataProvider model :: * -> *
  type ChildRelations model
  type ParentRelations model
  unpack ::
       DataProviderModel model -> PackingResult (ChildRelations model) -> model
  pack :: model -> ParentRelations model -> PackingResult model
  save :: model -> MonadDataProvider model model
  loadById :: Proxy model -> Int -> MonadDataProvider model (Maybe model)
  loadAll :: Proxy model -> MonadDataProvider model [model]
  deleteById :: Proxy model -> Int -> MonadDataProvider model (Either String ())

class HasDataSourceRun (actionMonad :: * -> *) (dsMonad :: * -> *) where
  runDS :: dsMonad a -> actionMonad a
