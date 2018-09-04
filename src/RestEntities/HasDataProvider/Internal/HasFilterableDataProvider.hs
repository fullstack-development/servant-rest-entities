{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module RestEntities.HasDataProvider.Internal.HasFilterableDataProvider where

import Data.Proxy
import GHC.TypeLits
import RestEntities.DataProvider
import RestEntities.Filter
import RestEntities.HasDataProvider.Internal.HasDataProvider
import RestEntities.HasDataProvider.Internal.HasLoadableDataProvider

class (HasLoadableDataProvider model) =>
      HasFilterableDataProvider model
  where
  getFilterField ::
       (KnownSymbol field, Eq (FilterFieldValue model field))
    => Proxy model
    -> Proxy field
    -> Filter model field
    -> (DataProviderModel model -> FilterFieldValue model field)
  --
  --
  filter ::
       (KnownSymbol field, Eq (FilterFieldValue model field))
    => [Filter model field]
    -> MonadDataProvider model [model]
  default filter :: (Filterable model field) =>
    [Filter model field] -> MonadDataProvider model [model]
  filter filters = do
    let selectors =
          map
            (\f@(ByEqField field value) ->
               (getFilterField (Proxy :: Proxy model) field f, value))
            filters
    entities <-
      getFilteredEntities (Proxy :: Proxy (DataProviderModel model)) selectors
    relations <- mapM (loadChildRelations (Proxy :: Proxy model)) entities
    let denormalized = zip entities relations
    return (uncurry unpack <$> denormalized)
