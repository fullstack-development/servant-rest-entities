{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module RestEntities.DataProvider where

import Data.Kind
import Data.Maybe
import Data.Proxy
import Data.Typeable
import Data.Void
import GHC.Generics
import GHC.TypeLits

data ChildRelationType
  = NoChild
  | SingularChild
  | MultipleChildren
  | VariousChildren

type EmptyChild = ChildRelation NoChild Void

type SingleChild a = ChildRelation SingularChild a

type ManyChildren a = ChildRelation MultipleChildren a

type Children a = ChildRelation VariousChildren a

data ChildRelation (childType :: ChildRelationType) a where
  ChildEmptyModel :: ChildRelation NoChild Void
  ChildModel :: model -> ChildRelation SingularChild model
  ChildrenModels :: [model] -> ChildRelation MultipleChildren model
  Nested
    :: ChildRelation ta a
    -> ChildRelation tb b
    -> ChildRelation VariousChildren (ChildRelation ta a, ChildRelation tb b)

type family DPModel model where
  DPModel (ChildRelation VariousChildren (a, b)) = ( DataProviderModel a
                                                   , DataProviderModel b)
  DPModel (ChildRelation MultipleChildren model) = DataProviderModel model
  DPModel (ChildRelation SingularChild model) = DataProviderModel model
  DPModel model = DataProviderModel model

type family DenormalizedWithChildren model where
  DenormalizedWithChildren (ChildRelation NoChild model) = ()
  DenormalizedWithChildren (ChildRelation SingularChild model) = ( DataProviderModel model
                                                                 , DenormalizedWithChildren (ChildRelations model))
  DenormalizedWithChildren (ChildRelation MultipleChildren model) = [( DataProviderModel model
                                                                     , DenormalizedWithChildren (ChildRelations model))]
  DenormalizedWithChildren (ChildRelation VariousChildren (a, b)) = ( DenormalizedWithChildren a
                                                                    , DenormalizedWithChildren b)
  DenormalizedWithChildren model = ( DataProviderModel model
                                   , DenormalizedWithChildren (ChildRelations model))

class HasRelation parent child where
  getPkSelector :: Proxy parent -> Proxy child -> (child -> Int)

class (DataProvider dp) =>
      HasRetrieveRelation (dp :: (* -> *)) a
  where
  type HasRetrieveRelationConstraint dp a :: Constraint
  getRelationById ::
       (HasRetrieveRelationConstraint dp a)
    => Proxy a
    -> Int
    -> (DPModel a -> Int)
    -> dp (DenormalizedWithChildren a)

instance (DataProvider dp) =>
         HasRetrieveRelation dp (ChildRelation NoChild a) where
  type HasRetrieveRelationConstraint dp (ChildRelation NoChild a) = Monad dp
  getRelationById _ pk _ = pure ()

instance ( DataProvider dp
         , HasDataProvider a
         , HasRetrieveRelation dp (ChildRelations a)
         , DPModel a ~ DataProviderModel a
         , DPModel (ChildRelations a) ~ DataProviderModel (ChildRelations a)
         ) =>
         HasRetrieveRelation dp (ChildRelation SingularChild a) where
  type HasRetrieveRelationConstraint dp (ChildRelation SingularChild a) = ( DataProviderTypeClass dp (DataProviderModel a)
                                                                          , HasRetrieveRelationConstraint dp (ChildRelations a))
  getRelationById _ pk pkSelector = do
    relations <- filterBySelector (Proxy :: Proxy (DPModel a)) pk pkSelector
    let Just relation = listToMaybe relations
    let childPkSelector =
          getPkSelector
            (Proxy :: Proxy (DPModel a))
            (Proxy :: Proxy (DPModel (ChildRelations a)))
    rels <-
      getRelationById (Proxy :: Proxy (ChildRelations a)) pk childPkSelector
    pure (relation, rels)

instance ( DataProvider dp
         , HasDataProvider a
         , HasRetrieveRelation dp (ChildRelations a)
         , DPModel a ~ DataProviderModel a
         ) =>
         HasRetrieveRelation dp (ChildRelation MultipleChildren a) where
  type HasRetrieveRelationConstraint dp (ChildRelation MultipleChildren a) = ( DataProviderTypeClass dp (DataProviderModel a)
                                                                             , HasRetrieveRelationConstraint dp (ChildRelations a))
  getRelationById _ pk pkSelector =
    filterBySelector (Proxy :: Proxy (DPModel a)) pk pkSelector >>=
    mapM loadChildren
    where
      loadChildren entity = do
        let childPkSelector =
              getPkSelector
                (Proxy :: Proxy (DPModel a))
                (Proxy :: Proxy (DPModel (ChildRelations a)))
        rels <-
          getRelationById (Proxy :: Proxy (ChildRelations a)) pk childPkSelector
        pure (entity, rels)

instance ( DataProvider dp
         , HasRetrieveRelation dp (ChildRelations a)
         , HasRetrieveRelation dp (ChildRelations b)
         , HasRelation (DPModel b) (DPModel (ChildRelations b))
         , HasRelation (DPModel a) (DPModel (ChildRelations a))
         ) =>
         HasRetrieveRelation dp (ChildRelation VariousChildren (a, b)) where
  type HasRetrieveRelationConstraint dp (ChildRelation VariousChildren (a, b)) = ( HasRetrieveRelationConstraint dp a
                                                                                 , HasRetrieveRelationConstraint dp b)
  getRelationById _ pk pkSelector = do
    entityA <- filterBySelector (Proxy :: Proxy (DPModel a)) pk pkSelector
    -- let childPkSelectorA =
    --       getPkSelector
    --         (Proxy :: Proxy (DPModel a))
    --         (Proxy :: Proxy (DPModel (ChildRelations a)))
    -- let childPkSelectorB =
    --       getPkSelector
    --         (Proxy :: Proxy (DPModel b))
    --         (Proxy :: Proxy (DPModel (ChildRelations b)))
    -- relationA <-
    --   getRelationById (Proxy :: Proxy (ChildRelations a)) pk childPkSelectorA
    -- relationB <-
    --   getRelationById (Proxy :: Proxy (ChildRelations b)) pk childPkSelectorB
    -- pure (relationA, relationB)
    pure undefined

type Loadable model
   = ( DataProvider (MonadDataProvider model)
     , DataProviderTypeClass (MonadDataProvider model) (DataProviderModel model))

type HasRelations model
   = ( DataProvider (MonadDataProvider model)
     , HasRetrieveRelation (MonadDataProvider model) (ChildRelations model)
     , HasRetrieveRelationConstraint (MonadDataProvider model) (ChildRelations model))

data Filter entity (field :: Symbol)
  = ByEqField (Proxy field)
              (FilterFieldValue entity field)
  | ByContainingFieldIn (Proxy field)
                        [FilterFieldValue entity field]
  deriving (Generic, Typeable)

type family FilterFieldValue entity (field :: Symbol)

class ( Monad (MonadDataProvider model)
      , DataProvider (MonadDataProvider model)
      , HasRelation (DPModel model) (DPModel (ChildRelations model))
      ) =>
      HasDataProvider model
  where
  type DataProviderModel model
  type MonadDataProvider model :: * -> *
  type ChildRelations model
  type ParentRelations model
  unpack ::
       DataProviderModel model
    -> DenormalizedWithChildren (ChildRelations model)
    -> model
  pack ::
       model
    -> ParentRelations model
    -> ( DataProviderModel model
       , DenormalizedWithChildren (ChildRelations model))
  getPK :: Proxy model -> DataProviderModel model -> Int
  save :: model -> MonadDataProvider model model
  --
  --
  loadById :: Proxy model -> Int -> MonadDataProvider model (Maybe model)
  default loadById :: Loadable model =>
    Proxy model -> Int -> MonadDataProvider model (Maybe model)
  loadById _ pk = do
    entity <- getEntityById (Proxy :: Proxy (DataProviderModel model)) pk
    case entity of
      Just e -> do
        relations <- getRelated (Proxy :: Proxy model) e
        return $ (`unpack` relations) <$> entity
      _ -> return Nothing
  --
  --
  loadAll :: Proxy model -> MonadDataProvider model [model]
  default loadAll :: Loadable model =>
    Proxy model -> MonadDataProvider model [model]
  loadAll proxyModel = do
    entities <- getAllEntities (Proxy :: Proxy (DataProviderModel model))
    relations <- mapM (getRelated (Proxy :: Proxy model)) entities
    let denormalized = zip entities relations
    let models = map (uncurry unpack) denormalized
    return models
  --
  --
  filter ::
       (KnownSymbol field)
    => [Filter model field]
    -> MonadDataProvider model [model]
  --
  --
  deleteById :: Proxy model -> Int -> MonadDataProvider model (Either String ())
  getRelated ::
       Proxy model
    -> DataProviderModel model
    -> MonadDataProvider model (DenormalizedWithChildren (ChildRelations model))
  default getRelated :: HasRelations model =>
    Proxy model -> DataProviderModel model -> MonadDataProvider model (DenormalizedWithChildren (ChildRelations model))
  getRelated proxyModel dpModel = do
    let pkSelector =
          getPkSelector
            (Proxy :: Proxy (DPModel model))
            (Proxy :: Proxy (DPModel (ChildRelations model)))
    let primaryKey = getPK proxyModel dpModel
    getRelationById
      (Proxy :: Proxy (ChildRelations model))
      primaryKey
      pkSelector

class HasDataSourceRun (actionMonad :: * -> *) (dsMonad :: * -> *) where
  runDS :: dsMonad a -> actionMonad a

-- Need to be implemented by concrete relevant data 
-- source backend (db, http services, memory and etc.)
class (Monad dp) =>
      DataProvider dp
  where
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
    => CreateDataStructure dp dbmodel
    -> dp (Maybe dbmodel)
  filterBySelector ::
       (DataProviderTypeClass dp dbmodel)
    => Proxy dbmodel
    -> value
    -> (dbmodel -> value)
    -> dp [dbmodel]
