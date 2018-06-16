{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebActions.Retrieve where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Void
import GHC.Generics
import Servant

import DBEntity
import Permissions
import Serializables

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
  data RetrieveActionView e
  retrieve :: Int -> MonadDB (DBModel e) (RetrieveActionView e)
  retrieve pk = do
    Just (dbModel, dbRels) <-
      getByIdWithRelsFromDB pk (Proxy :: Proxy (DBModel e))
    isAccessAllowed <- checkAccessPermission Nothing (Proxy :: Proxy e)
    unless isAccessAllowed (throwError err403)
    let model = dbConvertFrom dbModel (Just dbRels)
    isEntityAllowed <- checkEntityPermission Nothing model
    unless isEntityAllowed (throwError err403)
    let view = serialize model :: RetrieveActionView e
    pure view
  checkAccessPermission :: AccessPermissionCheck e Void
  checkAccessPermission _ _ = return True
  checkEntityPermission :: EntityPermissionCheck e Void
  checkEntityPermission _ _ = return True
