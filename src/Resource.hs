{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Resource where

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
  server :: Proxy e -> Server (Api e)
