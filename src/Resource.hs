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

import GHC.Generics
import Servant

import DBEntity

class (Generic e, DBConvertable e (DBModel e)) =>
      Resource e
  | e -> e
  where
  type Api e
  server :: Proxy e -> ServerT (Api e) (MonadDB (DBModel e))
