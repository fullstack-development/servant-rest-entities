{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebActions.List where

import Control.Monad.Except
import Data.Void
import GHC.Generics
import Servant

import DataProvider
import Permissions
import Serializables

class ( Generic e
      , Serializable e (ListActionView e)
      , HasDataProvider e
      , Monad (MonadDataProvider e)
      , MonadError ServantErr (MonadDataProvider e)
      , MonadIO (MonadDataProvider e)
      ) =>
      HasListMethod e
  | e -> e
  where
  data ListActionView e
  list :: MonadDataProvider e [ListActionView e]
  list = fmap serialize <$> loadAll (Proxy :: Proxy e)
  checkAccessPermission :: AccessPermissionCheck Void (MonadDataProvider e) e
  checkAccessPermission _ _ = return True
  checkEntityPermission :: EntityPermissionCheck Void (MonadDataProvider e) e
  checkEntityPermission _ _ = return True
