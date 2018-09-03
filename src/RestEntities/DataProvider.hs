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
{-# LANGUAGE DeriveGeneric #-}

module RestEntities.DataProvider where

import Data.Kind
import Data.Proxy
import Data.Typeable
import GHC.Generics
import GHC.TypeLits

data Filter entity (field :: Symbol)
  = ByEqField (Proxy field)
              (FilterFieldValue entity field)
  | ByContainingFieldIn (Proxy field)
                        [FilterFieldValue entity field]
  deriving (Generic, Typeable)

type family FilterFieldValue entity (field :: Symbol)

-- Need to be implemented by concrete relevant data 
-- source backend (db, http services, memory and etc.)
class (Monad dp) =>
      DataProvider dp
  where
  type DataProviderTypeClass dp :: * -> Constraint
  type CreateDataStructure dp :: * -> *
  getFilteredEntities ::
       (Eq value, DataProviderTypeClass dp dbmodel)
    => Proxy dbmodel
    -> [(dbmodel -> value, value)]
    -> dp [dbmodel]
  getAllEntities ::
       (DataProviderTypeClass dp dbmodel) => Proxy dbmodel -> dp [dbmodel]
  getEntityById ::
       (DataProviderTypeClass dp dbmodel)
    => Proxy dbmodel
    -> Int
    -> dp (Maybe dbmodel)
  createEntity ::
       (DataProviderTypeClass dp dbmodel)
    => CreateDataStructure dp dbmodel
    -> dp (Maybe dbmodel) -- TODO: add either here
  updateEntity :: (DataProviderTypeClass dp dbmodel) => dbmodel -> dp ()
