{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebActions.Update where

import GHC.Generics
import Servant

import Control.Monad.IO.Class
import DBEntity
import Serializables

class ( Generic e
      , Deserializable e (UpdateActionBody e)
      , Serializable e (UpdateActionView e)
      , DBConvertable e (DBModel e)
      , Monad (MonadDB (DBModel e))
      , MonadIO (MonadDB (DBModel e))
      ) =>
      HasUpdateMethod e
  | e -> e
  where
  data UpdateActionBody e
  data UpdateActionView e
  update ::
       Int -> UpdateActionBody e -> MonadDB (DBModel e) (UpdateActionView e)
  update entityId body = do
    model <- liftIO $ deserialize (Just entityId) body
    let dbModel = dbConvertTo model Nothing
    updatedDbModel <- save dbModel
    let updatedModel = dbConvertFrom updatedDbModel Nothing
    let view = serialize updatedModel
    pure view
