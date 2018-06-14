{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module Resource where

import Control.Monad.Except
import Control.Monad.IO.Class
import GHC.Generics
import Servant

import DBEntity
import Model
import Routing
import Serializables

class (Generic e, DBConvertable e (DBModel e)) =>
      Resource e
  | e -> e
  where
  type Api e
  server ::
       (Monad m, MonadIO m, MonadError ServantErr m)
    => Proxy e
    -> ServerT (Api e) m
