{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module RestEntities.Resource where

import GHC.Generics
import RestEntities.HasDataProvider
import Servant

class (Generic e, HasDataProvider e) =>
      Resource e
  where
  type Api e
  server :: Proxy e -> ServerT (Api e) (MonadDataProvider e)
