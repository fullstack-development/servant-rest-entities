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

module RestEntities.HasDataProvider.Internal.HasDataProvider where

import Data.Kind
import Data.Proxy
import Data.Void
import GHC.TypeLits
import RestEntities.DataProvider
import RestEntities.Model

type Filterable model field
   = (Eq (FilterFieldValue model field), KnownSymbol field, Loadable model)

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

type family DenormalizedWithChildren model where
  DenormalizedWithChildren (ChildRelation NoChild model) = ()
  DenormalizedWithChildren (ChildRelation SingularChild model) = ( DataProviderModel model
                                                                 , DenormalizedWithChildren (ChildRelations model))
  DenormalizedWithChildren (ChildRelation MultipleChildren model) = [( DataProviderModel model
                                                                     , DenormalizedWithChildren (ChildRelations model))]
  DenormalizedWithChildren (ChildRelation VariousChildren (a, b)) = ( DenormalizedWithChildren a
                                                                    , DenormalizedWithChildren b)
  DenormalizedWithChildren (ChildRelation VariousChildren (a, b, c)) = ( DenormalizedWithChildren a
                                                                       , DenormalizedWithChildren b
                                                                       , DenormalizedWithChildren c)
  DenormalizedWithChildren model = ( DataProviderModel model
                                   , DenormalizedWithChildren (ChildRelations model))

type family ParentRels model where
  ParentRels (ChildRelation NoChild model) = ()
  ParentRels (ChildRelation SingularChild model) = ParentRelations model
  ParentRels (ChildRelation MultipleChildren model) = [ParentRelations model]
  ParentRels (ChildRelation VariousChildren (a, b)) = ( ParentRelations a
                                                      , ParentRelations b)
  ParentRels (ChildRelation VariousChildren (a, b, c)) = ( ParentRelations a
                                                         , ParentRelations b
                                                         , ParentRelations c)
  ParentRels model = ParentRelations model

class (DataProvider dp) =>
      HasRetrieveRelation (dp :: (* -> *)) a
  where
  type HasRetrieveRelationConstraint dp a :: Constraint
  getRelationById ::
       (HasRetrieveRelationConstraint dp a)
    => Proxy a
    -> Int
    -> dp (DenormalizedWithChildren a)

instance (DataProvider dp) =>
         HasRetrieveRelation dp (ChildRelation NoChild a) where
  type HasRetrieveRelationConstraint dp (ChildRelation NoChild a) = Monad dp
  getRelationById _ pk = pure ()

instance ( DataProvider dp
         , HasDataProvider a
         , HasRetrieveRelation dp (ChildRelations a)
         ) =>
         HasRetrieveRelation dp (ChildRelation SingularChild a) where
  type HasRetrieveRelationConstraint dp (ChildRelation SingularChild a) = ( DataProviderTypeClass dp (DataProviderModel a)
                                                                          , HasRetrieveRelationConstraint dp (ChildRelations a))
  getRelationById _ pk = do
    Just relation <- getEntityById (Proxy :: Proxy (DataProviderModel a)) pk
    rels <- getRelationById (Proxy :: Proxy (ChildRelations a)) pk
    pure (relation, rels)

instance ( DataProvider dp
         , HasDataProvider a
         , HasRetrieveRelation dp (ChildRelations a)
         ) =>
         HasRetrieveRelation dp (ChildRelation MultipleChildren a) where
  type HasRetrieveRelationConstraint dp (ChildRelation MultipleChildren a) = ( DataProviderTypeClass dp (DataProviderModel a)
                                                                             , HasRetrieveRelationConstraint dp (ChildRelations a))
  getRelationById _ pk =
    getAllEntities (Proxy :: Proxy (DataProviderModel a)) >>= mapM loadChildren
    where
      loadChildren entity = do
        rels <- getRelationById (Proxy :: Proxy (ChildRelations a)) pk
        pure (entity, rels)

instance (HasRetrieveRelation dp a, HasRetrieveRelation dp b) =>
         HasRetrieveRelation dp (ChildRelation VariousChildren (a, b)) where
  type HasRetrieveRelationConstraint dp (ChildRelation VariousChildren (a, b)) = ( HasRetrieveRelationConstraint dp a
                                                                                 , HasRetrieveRelationConstraint dp b)
  getRelationById _ pk = do
    relationA <- getRelationById (Proxy :: Proxy a) pk
    relationB <- getRelationById (Proxy :: Proxy b) pk
    pure (relationA, relationB)

type Loadable model
   = ( DataProvider (MonadDataProvider model)
     , DataProviderTypeClass (MonadDataProvider model) (DataProviderModel model))

type HasRelations model
   = ( DataProvider (MonadDataProvider model)
     , HasRetrieveRelation (MonadDataProvider model) (ChildRelations model)
     , HasRetrieveRelationConstraint (MonadDataProvider model) (ChildRelations model))

class (Monad (MonadDataProvider model), DataProvider (MonadDataProvider model)) =>
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
    -> (ParentRelations model, ParentRels (ChildRelations model))
    -> ( DataProviderModel model
       , DenormalizedWithChildren (ChildRelations model))
  getPK :: Proxy model -> DataProviderModel model -> Int
  getID :: model -> Id Int
  prepareToCreate ::
       Proxy model
    -> DataProviderModel model
    -> CreateDataStructure (MonadDataProvider model) (DataProviderModel model)
