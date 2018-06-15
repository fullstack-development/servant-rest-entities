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
  -- <<<<<<< HEAD
  --   list :: MonadDB (DBModel e) [ListActionView e]
  --   list = do
  --     dbModels <- getAllEntities (Proxy :: Proxy (DBModel e))
  --     let entityModels = fmap (`dbConvertFrom` Nothing) dbModels
  --     let view = map serialize entityModels
  --     pure view
  -- =======
  list :: MonadDB (DBModel e) [ListActionView e]
  list = undefined
    -- do
    -- dbEntities <-
    --   liftIO
    --     (getAllFromDBWithRels :: IO [(DBModel e, DBModel (ChildRelations e))])
    -- return $ serialize . convertToModels <$> dbEntities
    -- where
    --   convertToModels (e, rels) = dbConvertFrom e (Just rels)
-- >>>>>>> 95923502a686085ff86057fcbe4f79e8839f7405
