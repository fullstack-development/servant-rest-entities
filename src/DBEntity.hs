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

class (DBEntity to) =>
      DBConvertable e to
  | e -> to
  , to -> e
  where
  type DBModel e
  type Relations e
  dbConvertTo :: e -> Maybe (Relations e) -> to
  dbConvertFrom :: to -> Maybe (Relations e) -> e

class DBEntity e where
  save :: e -> IO e
  deleteFromDB :: Proxy e -> Int -> IO (Either String ())
