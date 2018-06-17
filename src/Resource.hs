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

import Routing

class (Generic e) =>
      Resource e
  | e -> e
  where
  type Api e
  type MonadWeb e :: * -> *
  server :: Proxy e -> ServerT (Api e) (MonadWeb e)
