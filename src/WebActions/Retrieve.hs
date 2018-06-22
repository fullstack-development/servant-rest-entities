{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebActions.Retrieve where

import Control.Monad
import Control.Monad.Except
import Data.Maybe
import GHC.Generics

import DBEntity
import DataProvider
import Permissions
import Serializables
import Servant
import Servant.Auth.Server

class ( Generic e
      , Serializable e (RetrieveActionView e)
      , HasDataProvider e (DataProviderModel e)
      , Monad (MonadDataProvider e)
      , MonadIO (MonadDataProvider e)
      , MonadError ServantErr (MonadDataProvider e)
      ) =>
      HasRetrieveMethod e
  | e -> e
  where
  type Requester e
  data RetrieveActionView e
  retrieve' ::
       AuthResult (Requester e)
    -> Int
    -> MonadDataProvider e (RetrieveActionView e)
  retrieve' (Authenticated requester) pk = do
    isAccessAllowed <- checkAccessPermission (Proxy :: Proxy e) (Just requester)
    unless isAccessAllowed (throwError err403)
    mbEntity <- loadById (Proxy :: Proxy e) pk
    when (isNothing mbEntity) (throwError err404)
    let entity = fromJust mbEntity
    isEntityAllowed <- checkEntityPermission (Just requester) entity
    unless isEntityAllowed (throwError err403)
    pure (serialize entity :: RetrieveActionView e)
  retrieve' _ _ = throwError err401
  retrieve :: Int -> MonadDataProvider e (RetrieveActionView e)
  retrieve pk = do
    mbEntity <- loadById (Proxy :: Proxy e) pk
    when (isNothing mbEntity) (throwError err404)
    pure (serialize (fromJust mbEntity) :: RetrieveActionView e)
  checkAccessPermission ::
       AccessPermissionCheck (Requester e) (MonadDataProvider e) e
  checkAccessPermission _ _ = return True
  checkEntityPermission ::
       EntityPermissionCheck (Requester e) (MonadDataProvider e) e
  checkEntityPermission _ _ = return True
