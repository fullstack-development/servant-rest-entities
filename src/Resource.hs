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

import DataProvider
import GHC.Generics
import Servant

class (Generic e, HasDataProvider e (DataProviderModel e)) =>
      Resource e
  | e -> e
  where
  type Api e
  server :: Proxy e -> ServerT (Api e) (MonadDataProvider e)
