{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebActions.List where

import Control.Monad
import Control.Monad.Except
import Data.Void
import GHC.Generics
import Servant

import DBEntity
import Permissions
import Serializables

class ( Generic e
      , Serializable e (ListActionView e)
      , DBConvertable e (DBModel e)
      , Monad (MonadDB (DBModel e))
      , MonadError ServantErr (MonadDB (DBModel e))
      ) =>
      HasListMethod e
  | e -> e
  where
  data ListActionView e
  list :: MonadDB (DBModel e) [ListActionView e]
  list = do
    isAccessAllowed <- checkAccessPermission Nothing (Proxy :: Proxy e)
    dbEntities <-
      getAllFromDBWithRels :: MonadDB (DBModel e) [( DBModel e
                                                   , DBModel (ChildRelations e))]
    let models = convertToModels <$> dbEntities
    isEntitiesAllowed <- and <$> mapM (checkEntityPermission Nothing) models
    unless isEntitiesAllowed (throwError err401)
    return $ serialize <$> models
    where
      convertToModels (e, rels) = dbConvertFrom e (Just rels)
  checkAccessPermission :: AccessPermissionCheck e Void
  checkAccessPermission _ _ = return True
  checkEntityPermission :: EntityPermissionCheck e Void
  checkEntityPermission _ _ = return True
