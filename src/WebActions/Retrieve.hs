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
      HasRetrieveMethod e requester
  | e -> requester
  where
  data RetrieveActionView e
  retrieve' ::
       Proxy e
    -> AuthResult requester
    -> Int
    -> MonadDB (DBModel e) (RetrieveActionView e)
  retrieve' p (Authenticated requester) pk = do
    Just (dbModel, dbRels) <-
      getByIdWithRelsFromDB pk (Proxy :: Proxy (DBModel e))
    isAccessAllowed <- checkAccessPermission (Just requester) p
    unless isAccessAllowed (throwError err403)
    let model = dbConvertFrom dbModel (Just dbRels)
    isEntityAllowed <- checkEntityPermission (Just requester) model
    unless isEntityAllowed (throwError err403)
    pure (serialize model :: RetrieveActionView e)
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
  checkAccessPermission :: AccessPermissionCheck e requester
  checkAccessPermission _ _ = return True
  checkEntityPermission :: EntityPermissionCheck e requester
  checkEntityPermission _ _ = return True
