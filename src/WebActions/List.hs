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
      ) =>
      HasListMethod e
  | e -> e
  where
  data ListActionView e
  list :: Handler [ListActionView e]
  list = do
    dbEntities <-
      liftIO
        (getAllFromDBWithRels :: IO [(DBModel e, DBModel (ChildRelations e))])
    return $ serialize . convertToModels <$> dbEntities
    where
      convertToModels (e, rels) = dbConvertFrom e (Just rels)
