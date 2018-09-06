{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RestEntities.WebActions.List where

import Control.Monad.Except
import Data.Void
import GHC.Generics
import Servant

import RestEntities.HasDataProvider.HasDataProvider
import RestEntities.Permissions
import RestEntities.Serializables

class ( Generic e
      , Serializable e (ListActionView e)
      , HasLoadableDataProvider e
      , Monad (MonadDataProvider e)
      , MonadError ServantErr (MonadDataProvider e)
      , MonadIO (MonadDataProvider e)
      ) =>
      HasListMethod e
  where
  type ListActionView e = l | l -> e
  list :: MonadDataProvider e [ListActionView e]
  list = fmap serialize <$> loadAll (Proxy :: Proxy e)
  checkAccessPermission :: AccessPermissionCheck Void (MonadDataProvider e) e
  checkAccessPermission _ _ = return True
  checkEntityPermission :: EntityPermissionCheck Void (MonadDataProvider e) e
  checkEntityPermission _ _ = return True
