{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module RestEntities.HasDataProvider.Internal.HasDeleteableDataProvider where

import Data.Proxy
import RestEntities.HasDataProvider.Internal.HasDataProvider

class (HasDataProvider model) =>
      HasDeleteableDataProvider model
  where
  deleteById :: Proxy model -> Int -> MonadDataProvider model (Either String ())
