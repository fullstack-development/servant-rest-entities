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
      ) =>
      HasRetrieveMethod e
  | e -> e
  where
  data RetrieveActionView e
  retrieve :: (Monad m, MonadIO m) => Int -> m (RetrieveActionView e)
  retrieve = undefined
