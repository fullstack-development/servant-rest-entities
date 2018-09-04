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

module RestEntities.HasDataProvider.Internal.HasLoadableDataProvider where

import Data.Proxy
import RestEntities.HasDataProvider.Internal.HasDataProvider
import RestEntities.DataProvider

class (HasDataProvider model) =>
      HasLoadableDataProvider model
  where
  loadParentRelations ::
       model
    -> MonadDataProvider model ( ParentRelations model
                               , ParentRels (ChildRelations model))
  loadById :: Proxy model -> Int -> MonadDataProvider model (Maybe model)
  default loadById :: Loadable model =>
    Proxy model -> Int -> MonadDataProvider model (Maybe model)
  loadById _ pk = do
    entity <- getEntityById (Proxy :: Proxy (DataProviderModel model)) pk
    case entity of
      Just e -> do
        relations <- loadChildRelations (Proxy :: Proxy model) e
        return $ (`unpack` relations) <$> entity
      _ -> return Nothing
  loadAll :: Proxy model -> MonadDataProvider model [model]
  default loadAll :: Loadable model =>
    Proxy model -> MonadDataProvider model [model]
  loadAll proxyModel = do
    entities <- getAllEntities (Proxy :: Proxy (DataProviderModel model))
    relations <- mapM (loadChildRelations (Proxy :: Proxy model)) entities
    let denormalized = zip entities relations
    let models = map (uncurry unpack) denormalized
    return models
  loadChildRelations ::
       Proxy model
    -> DataProviderModel model
    -> MonadDataProvider model (DenormalizedWithChildren (ChildRelations model))
  default loadChildRelations :: HasRelations model =>
    Proxy model -> DataProviderModel model -> MonadDataProvider model (DenormalizedWithChildren (ChildRelations model))
  loadChildRelations proxyModel dpModel = do
    let primaryKey = getPK proxyModel dpModel
    getRelationById (Proxy :: Proxy (ChildRelations model)) primaryKey