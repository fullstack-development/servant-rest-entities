{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RestEntities.WebActions.Update where

import Control.Monad.Except
import Data.Maybe
import Data.Void
import GHC.Generics
import Servant

import RestEntities.HasDataProvider.HasDataProvider
import RestEntities.Permissions
import RestEntities.Serializables

class ( Generic e
      , Deserializable e (UpdateActionBody e)
      , Serializable e (UpdateActionView e)
      , HasLoadableDataProvider e
      , HasSaveableDataProvider e
      , Monad (MonadDataProvider e)
      , MonadIO (MonadDataProvider e)
      , MonadError ServantErr (MonadDataProvider e)
      ) =>
      HasUpdateMethod e
  where
  type UpdateActionBody e = uAb | uAb -> e
  type UpdateActionView e = uAv | uAv -> e
  update ::
       Int -> UpdateActionBody e -> (MonadDataProvider e) (UpdateActionView e)
  update entityId body = do
    modelUpdates <- liftIO $ deserialize (Just entityId) body -- Get updates
    mbExistingModel <- loadById (Proxy :: Proxy e) entityId
    when (isNothing mbExistingModel) (throwError err404)
    let existingModel = fromJust mbExistingModel
    let updatedUnsavedModel = existingModel -- TODO: Do update here - existingModel `patchBy` modelUpdates
    updatedModel <- save updatedUnsavedModel Nothing
    return (serialize updatedModel :: UpdateActionView e)
  checkAccessPermission :: AccessPermissionCheck Void (MonadDataProvider e) e
  checkAccessPermission _ _ = return True
  checkEntityPermission :: EntityPermissionCheck Void (MonadDataProvider e) e
  checkEntityPermission _ _ = return True
