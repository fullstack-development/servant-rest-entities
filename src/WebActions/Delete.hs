{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebActions.Delete where

import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics
import Servant

import DBEntity
import Serializables

class ( Generic e
      , DBConvertable e (DBModel e)
      , MonadIO (MonadDB (DBModel e))
      , MonadError ServantErr (MonadDB (DBModel e))
      ) =>
      HasDeleteMethod e
  | e -> e
  where
  delete :: Proxy e -> Int -> MonadDB (DBModel e) ()
  delete proxyType entityId = do
    result <- deleteFromDB (Proxy :: Proxy (DBModel e)) entityId
    case result of
      Left err -> throwError $ err400 {errBody = BL.pack err}
      Right () -> pure ()
