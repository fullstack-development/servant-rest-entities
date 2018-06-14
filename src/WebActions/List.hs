{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebActions.List where

import Control.Monad.IO.Class
import GHC.Generics
import Servant

import DBEntity
import Serializables

class ( Generic e
      , Serializable e (ListActionView e)
      , DBConvertable e (DBModel e)
      ) =>
      HasListMethod e
  | e -> e
  where
  data ListActionView e
  list :: (Monad m, MonadIO m) => m [ListActionView e]
  list = do
    dbModels <- liftIO $ getAllEntities (Proxy :: Proxy (DBModel e))
    let entityModels = fmap (`dbConvertFrom` Nothing) dbModels
    let view = map serialize entityModels
    pure view
