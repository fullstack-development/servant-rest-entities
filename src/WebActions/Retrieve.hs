{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebActions.Retrieve where

import Control.Monad.IO.Class
import GHC.Generics
import Servant

import DBEntity
import Serializables

class ( Generic e
      , Serializable e (RetrieveActionView e)
      , DBConvertable e (DBModel e)
      , Monad (MonadDB (DBModel e))
      ) =>
      HasRetrieveMethod e
  | e -> e
  where
  data RetrieveActionView e
  retrieve :: Int -> MonadDB (DBModel e) (RetrieveActionView e)
  retrieve pk = do
    Just (dbModel, dbRels) <-
      getByIdWithRelsFromDB pk (Proxy :: Proxy (DBModel e))
    let model = dbConvertFrom dbModel (Just dbRels)
    let view = serialize model :: RetrieveActionView e
    pure view
