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
import Data.Proxy
import Resource
import Serializables

class ( Generic e
      , Deserializable e (UpdateActionBody e)
      , Serializable e (UpdateActionView e)
      , DBConvertable e (DBModel e)
      , Monad (MonadWeb e)
      , MonadIO (MonadWeb e)
      ) =>
      HasUpdateMethod e
  | e -> e
  where
  data UpdateActionBody e
  data UpdateActionView e
  update :: Int -> UpdateActionBody e -> MonadWeb e (UpdateActionView e)
  update entityId body = do
    modelUpdates <- liftIO $ deserialize (Just entityId) body -- Get updates
    -- Get entity with all her relations
    Just (dbModel, dbModelRels) <-
      getByIdWithRelsFromDB entityId (Proxy :: Proxy (DBModel e))
    -- Obtain updated model
    let existingModel = dbConvertFrom dbModel (Just dbModelRels)
    let updatedModel = existingModel -- TODO: Do update here - existingModel `patchBy` modelUpdates
    -- Obtain new dbmodel
    let (dbModel, dbRels) = dbConvertTo updatedModel Nothing
    updatedDbModel <- save dbModel
    let updatedModel = dbConvertFrom updatedDbModel (Just dbRels)
    let view = serialize updatedModel :: UpdateActionView e
    pure view
