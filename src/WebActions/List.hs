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
import Serializables

class ( Generic e
      , Serializable e (ListActionView e)
      , DBConvertable e (DBModel e)
      , Monad (MonadDB (DBModel e))
      ) =>
      HasListMethod e
  | e -> e
  where
  data ListActionView e
  list :: MonadDB (DBModel e) [ListActionView e]
  list = do
    dbEntities <-
      getAllFromDBWithRels :: MonadDB (DBModel e) [( DBModel e
                                                   , DBModel (ChildRelations e))]
    return $ serialize . convertToModels <$> dbEntities
    where
      convertToModels (e, rels) = dbConvertFrom e (Just rels)
