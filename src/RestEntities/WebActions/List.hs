{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RestEntities.WebActions.List where

import Control.Monad.Except
import Data.Void
import GHC.Generics
import Servant

import RestEntities.HasDataProvider
import RestEntities.Permissions
import RestEntities.Serializables

class ( Generic e
      , Serializable e (ListActionView e)
      , HasDataProviderLoadable e
      , Monad (MonadDataProvider e)
      , MonadError ServantErr (MonadDataProvider e)
      , MonadIO (MonadDataProvider e)
      ) =>
      HasListMethod e
  where
  data ListActionView e
  list :: MonadDataProvider e [ListActionView e]
  list = fmap serialize <$> loadAll (Proxy :: Proxy e)
  checkAccessPermission :: AccessPermissionCheck Void (MonadDataProvider e) e
  checkAccessPermission _ _ = return True
  checkEntityPermission :: EntityPermissionCheck Void (MonadDataProvider e) e
  checkEntityPermission _ _ = return True
