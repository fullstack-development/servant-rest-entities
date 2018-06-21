{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}

module WebActions.Create where

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
      , Deserializable e (CreateActionBody e)
      , Serializable e (CreateActionView e)
      , DBConvertable e (DBModel e)
      , MonadIO (MonadDB (DBModel e))
      , MonadError ServantErr (MonadDB (DBModel e))
      ) =>
      HasCreateMethod e
  | e -> e
  where
  type Requester e
  data CreateActionBody e
  data CreateActionView e
  create :: CreateActionBody e -> MonadDB (DBModel e) (CreateActionView e)
  create body = do
    isAccessAllowed <- checkAccessPermission Nothing (Proxy :: Proxy e)
    unless isAccessAllowed (throwError err401)
    model <- liftIO $ deserialize Nothing body
    let (dbModel, dbRels) = dbConvertTo model Nothing
    newDbModel <- save dbModel
    let newModel = dbConvertFrom newDbModel Nothing
    let view = serialize newModel
    pure view
  checkAccessPermission :: AccessPermissionCheck e (Requester e)
  checkAccessPermission _ _ = return True
