{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module DBEntity where

import Data.Proxy (Proxy(Proxy))

type family DBModel e

type instance DBModel () = ()

type instance DBModel (a, b) = (DBModel a, DBModel b)

type instance DBModel (a, b, c) = (DBModel a, DBModel b, DBModel c)

type instance DBModel (a, b, c, d) =
     (DBModel a, DBModel b, DBModel c, DBModel d)

type instance DBModel (a, b, c, d, e) =
     (DBModel a, DBModel b, DBModel c, DBModel d, DBModel e)

type instance DBModel (a, b, c, d, e, f) =
     (DBModel a, DBModel b, DBModel c, DBModel d, DBModel e, DBModel f)

type instance DBModel (a, b, c, d, e, f, h) =
     (DBModel a, DBModel b, DBModel c, DBModel d, DBModel e, DBModel f,
      DBModel h)

class (DBEntity e to) =>
      DBConvertable e to
  | e -> to
  , to -> e
  where
  type ChildRelations e
  type ParentRelations e
  dbConvertTo ::
       e -> Maybe (ParentRelations e) -> (to, DBModel (ChildRelations e))
  dbConvertFrom :: to -> Maybe (DBModel (ChildRelations e)) -> e

class DBEntity model dbmodel | dbmodel -> model where
  save :: dbmodel -> IO dbmodel
  getByIdFromDB :: Int -> IO (Maybe dbmodel)
  getByIdWithRelsFromDB ::
       Int
    -> Proxy dbmodel
    -> IO (Maybe (dbmodel, DBModel (ChildRelations model)))
  deleteFromDB :: Proxy dbmodel -> Int -> IO (Either String ())
  getAllFromDB :: IO [dbmodel]
  getAllFromDBWithRels :: IO [(dbmodel, DBModel (ChildRelations model))]
