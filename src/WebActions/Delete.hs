{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebActions.Delete where

import Control.Monad.Except
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Void
import GHC.Generics
import Servant

import DataProvider
import Permissions

class ( Generic e
      , HasDataProvider e
      , MonadIO (MonadDataProvider e)
      , MonadError ServantErr (MonadDataProvider e)
      ) =>
      HasDeleteMethod e
  | e -> e
  where
  delete :: Proxy e -> Int -> MonadDataProvider e ()
  delete proxyType entityId = do
    result <- deleteById (Proxy :: Proxy e) entityId
    case result of
      Left err -> throwError $ err400 {errBody = BL.pack err}
      Right () -> pure ()
  checkAccessPermission :: AccessPermissionCheck Void (MonadDataProvider e) e
  checkAccessPermission _ _ = return True
