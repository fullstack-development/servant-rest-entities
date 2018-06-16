{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebActions.Update where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Proxy
import Data.Void
import GHC.Generics
import Servant

import DBEntity
import Permissions
import Serializables

class ( Generic e
      , Deserializable e (UpdateActionBody e)
      , Serializable e (UpdateActionView e)
      , DBConvertable e (DBModel e)
      , Monad (MonadDB (DBModel e))
      , MonadIO (MonadDB (DBModel e))
      , MonadError ServantErr (MonadDB (DBModel e))
      ) =>
      HasUpdateMethod e
  | e -> e
  where
  data UpdateActionBody e
  data UpdateActionView e
  update ::
       Int -> UpdateActionBody e -> MonadDB (DBModel e) (UpdateActionView e)
  update entityId body = do
    isAccessAllowed <- checkAccessPermission Nothing (Proxy :: Proxy e)
    unless isAccessAllowed (throwError err401)
    modelUpdates <- liftIO $ deserialize (Just entityId) body -- Get updates
    -- Get entity with all her relations
    Just (dbModel, dbModelRels) <-
      getByIdWithRelsFromDB entityId (Proxy :: Proxy (DBModel e))
    -- Obtain updated model
    let existingModel = dbConvertFrom dbModel (Just dbModelRels)
    isEntityAllowed <- checkEntityPermission Nothing existingModel
    unless isEntityAllowed (throwError err401)
    let updatedModel = existingModel -- TODO: Do update here - existingModel `patchBy` modelUpdates
    -- Obtain new dbmodel
    let (dbModel, dbRels) = dbConvertTo updatedModel Nothing
    updatedDbModel <- save dbModel
    let updatedModel = dbConvertFrom updatedDbModel (Just dbRels)
    let view = serialize updatedModel :: UpdateActionView e
    pure view
  checkAccessPermission :: AccessPermissionCheck e Void
  checkAccessPermission _ _ = return True
  checkEntityPermission :: EntityPermissionCheck e Void
  checkEntityPermission _ _ = return True
