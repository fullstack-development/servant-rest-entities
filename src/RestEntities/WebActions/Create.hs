{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RestEntities.WebActions.Create where

import Control.Monad.Except
import GHC.Generics
import Servant

import RestEntities.DataProvider
import RestEntities.Permissions
import RestEntities.Serializables

class ( Generic e
      , Deserializable e (CreateActionBody e)
      , Serializable e (CreateActionView e)
      , HasDataProvider e
      , MonadIO (MonadDataProvider e)
      , MonadError ServantErr (MonadDataProvider e)
      ) =>
      HasCreateMethod e
  where
  type Requester e
  data CreateActionBody e
  data CreateActionView e
  create :: CreateActionBody e -> MonadDataProvider e (CreateActionView e)
  create body = do
    model <- liftIO $ deserialize Nothing body
    created <- save model Nothing
    pure . serialize $ created
  checkAccessPermission ::
       AccessPermissionCheck (Requester e) (MonadDataProvider e) e
  checkAccessPermission _ _ = return True
