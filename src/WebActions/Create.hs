{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}

module WebActions.Create where

import Control.Monad.IO.Class
import GHC.Generics
import Servant

import DBEntity
import Serializables

class ( Generic e
      , Deserializable e (CreateActionBody e)
      , Serializable e (CreateActionView e)
      , DBConvertable e (DBModel e)
      ) =>
      HasCreateMethod e
  | e -> e
  where
  data CreateActionBody e
  data CreateActionView e
  create :: (Monad m, MonadIO m) => CreateActionBody e -> m (CreateActionView e)
  create body = do
    model <- liftIO $ deserialize Nothing body
    let dbModel = dbConvertTo model Nothing
    newDbModel <- liftIO $ save dbModel
    let newModel = dbConvertFrom newDbModel Nothing
    let view = serialize newModel
    pure view
