{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebActions.Retrieve where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import DBEntity
import Data.Void
import GHC.Generics
import Permissions
import Serializables
import Servant
import Servant.Auth.Server

class ( Generic e
      , Serializable e (RetrieveActionView e)
      , DBConvertable e (DBModel e)
      , Monad (MonadDB (DBModel e))
      , MonadIO (MonadDB (DBModel e))
      , MonadError ServantErr (MonadDB (DBModel e))
      ) =>
      HasRetrieveMethod e
  | e -> e
  where
  type Requester e
  data RetrieveActionView e
  retrieve' ::
       AuthResult (Requester e)
    -> Int
    -> MonadDB (DBModel e) (RetrieveActionView e)
  retrieve' (Authenticated requester) pk = do
    Just (dbModel, dbRels) <-
      getByIdWithRelsFromDB pk (Proxy :: Proxy (DBModel e))
    isAccessAllowed <- checkAccessPermission (Just requester) (Proxy :: Proxy e)
    unless isAccessAllowed (throwError err403)
    let model = dbConvertFrom dbModel (Just dbRels)
    isEntityAllowed <- checkEntityPermission (Just requester) model
    unless isEntityAllowed (throwError err403)
    pure (serialize model :: RetrieveActionView e)
  retrieve' _ _ = throwError err401
  retrieve :: Int -> MonadDB (DBModel e) (RetrieveActionView e)
  retrieve pk = do
    Just (dbModel, dbRels) <-
      getByIdWithRelsFromDB pk (Proxy :: Proxy (DBModel e))
    isAccessAllowed <- checkAccessPermission Nothing (Proxy :: Proxy e)
    unless isAccessAllowed (throwError err403)
    let model = dbConvertFrom dbModel (Just dbRels)
    isEntityAllowed <- checkEntityPermission Nothing model
    unless isEntityAllowed (throwError err403)
    pure (serialize model :: RetrieveActionView e)
  checkAccessPermission :: AccessPermissionCheck e (Requester e)
  checkAccessPermission _ _ = return True
  checkEntityPermission :: EntityPermissionCheck e (Requester e)
  checkEntityPermission _ _ = return True
