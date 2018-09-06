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

module RestEntities.HasDataProvider.Internal.ChildRelation where

import Data.Void

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
