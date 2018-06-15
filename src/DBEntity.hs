{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module DBEntity where

import Control.Monad.IO.Class (MonadIO)
import Data.Proxy (Proxy(Proxy))

class HasDbRun (actionMonad :: * -> *) (dbMonad :: * -> *) where
  runDB :: (dbMonad a) -> actionMonad a

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
  type MonadDB e :: * -> *
  save :: e -> MonadDB e e
  getAllEntities :: Proxy e -> MonadDB e [e]
  deleteFromDB :: Proxy e -> Int -> MonadDB e (Either String ())
