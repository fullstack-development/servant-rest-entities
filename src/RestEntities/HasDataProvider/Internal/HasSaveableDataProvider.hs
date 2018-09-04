{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module RestEntities.HasDataProvider.Internal.HasSaveableDataProvider where

import Data.Proxy
import RestEntities.DataProvider
import RestEntities.HasDataProvider.Internal.HasDataProvider
import RestEntities.HasDataProvider.Internal.HasLoadableDataProvider
import RestEntities.Model

class (HasDataProvider model) =>
      HasSaveableDataProvider model
  where
  save ::
       model
    -> Maybe (ParentRelations model, ParentRels (ChildRelations model))
    -> MonadDataProvider model model
  default save :: (Loadable model, HasLoadableDataProvider model) =>
    model -> Maybe (ParentRelations model, ParentRels (ChildRelations model)) -> MonadDataProvider model model
  save entity mbParents = do
    parents <- maybe (loadParentRelations entity) return mbParents
    let (dbentity, rels) = pack entity parents
    savedEntity <-
      if isIdEmpty $ getID entity
        then do
          let err = error "Error while creating entity after"
          let createStructure = prepareToCreate (Proxy :: Proxy model) dbentity
          res <- createEntity createStructure
          maybe err return res
        else updateEntity dbentity >> return dbentity
    let pk = getPK (Proxy :: Proxy model) savedEntity
    freshEntity <- loadById (Proxy :: Proxy model) pk
    case freshEntity of
      Just c -> return c
      _ -> error "Error while retrieving fresh entity after saving it"
