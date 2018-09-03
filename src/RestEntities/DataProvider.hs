{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module RestEntities.DataProvider where

import Data.Kind
import Data.Proxy
import Data.Typeable
import Data.Void
import GHC.Generics
import GHC.TypeLits
import RestEntities.Model

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

type Filterable model field
   = (Eq (FilterFieldValue model field), KnownSymbol field, Loadable model)

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
  getFilterField ::
       (KnownSymbol field, Eq (FilterFieldValue model field), Eq (FilterFieldValue (DataProviderModel model) field))
    => Proxy model
    -> Proxy field
    -> Filter model field
    -> Filter (DataProviderModel model) field
  --
  --
  filter ::
       (KnownSymbol field, Eq (FilterFieldValue model field), Eq (FilterFieldValue (DataProviderModel model) field))
    => [Filter model field]
    -> MonadDataProvider model [model]
  default filter :: (KnownSymbol field, Eq (FilterFieldValue model field), Eq (FilterFieldValue (DataProviderModel model) field), Filterable model field) =>
    [Filter model field] -> MonadDataProvider model [model]
  filter filters = do
    let selectors =
          map 
           (\f@(ByEqField field value) ->
               getFilterField (Proxy :: Proxy model) field f)
             filters
    entities <-
      getFilteredEntities (Proxy :: Proxy (DataProviderModel model)) selectors
    relations <- mapM (loadChildRelations (Proxy :: Proxy model)) entities
    let denormalized = zip entities relations
    return (uncurry unpack <$> denormalized)
  --
  --
  deleteById :: Proxy model -> Int -> MonadDataProvider model (Either String ())
  loadParentRelations ::
       model
    -> MonadDataProvider model ( ParentRelations model
                               , ParentRels (ChildRelations model))
  --
  --
  default save :: Loadable model =>
    model -> Maybe (ParentRelations model, ParentRels (ChildRelations model)) -> MonadDataProvider model model
  save ::
       model
    -> Maybe (ParentRelations model, ParentRels (ChildRelations model))
    -> MonadDataProvider model model
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
  --
  --
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
  --
  --
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
--
class HasDataSourceRun (actionMonad :: * -> *) (dsMonad :: * -> *) where
  runDS :: dsMonad a -> actionMonad a

-- Need to be implemented by concrete relevant data 
-- source backend (db, http services, memory and etc.)
class (Monad dp) =>
      DataProvider dp
  where
  type DataProviderTypeClass dp :: * -> Constraint
  type CreateDataStructure dp :: * -> *
  getFilteredEntities :: (DataProviderTypeClass dp dbmodel, Eq (FilterFieldValue dbmodel field), KnownSymbol field) =>
    Proxy dbmodel -> [Filter dbmodel field] -> dp [dbmodel]
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