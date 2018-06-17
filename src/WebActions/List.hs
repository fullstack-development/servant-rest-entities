{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebActions.List where

import Control.Monad.IO.Class
import GHC.Generics
import Servant

import DBEntity
import Resource
import Serializables

class ( Generic e
      , Serializable e (ListActionView e)
      , DBConvertable e (DBModel e)
      , Monad (MonadWeb e)
      , MonadIO (MonadWeb e)
      ) =>
      HasListMethod e
  | e -> e
  where
  data ListActionView e
  list :: MonadWeb e [ListActionView e]
  list = do
    dbEntities <- getAllFromDBWithRels
    return $ serialize . convertToModels <$> dbEntities
    where
      convertToModels (e, rels) = dbConvertFrom e (Just rels)
