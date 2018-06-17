{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Examples.UsersWithBeamDB.Resources where

import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import Database.Beam.Backend.SQL.Types (unSerial)
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant

import Examples.UsersWithBeamDB.Model
import Examples.UsersWithBeamDB.ServerConfig
import Model
import Resource
import Routing
import Serializables
import WebActions.Create
import WebActions.Delete
import WebActions.List
import WebActions.Retrieve
import WebActions.Update

instance Resource User where
  type Api User = CreateApi "users" (CreateActionBody User) (CreateActionView User) :<|> DeleteApi "users" :<|> UpdateApi "users" (UpdateActionBody User) (UpdateActionView User) :<|> ListApi "users" (ListActionView User) :<|> RetrieveApi "users" (RetrieveActionView User)
  type MonadWeb User = ServerConfigReader
  server :: Proxy User -> ServerT (Api User) ServerConfigReader
  server proxyEntity = userServerApi

instance Resource Auth where
  type Api Auth = UpdateApi "auth" (UpdateActionBody Auth) (UpdateActionView Auth)
  type MonadWeb Auth = ServerConfigReader
  server :: Proxy Auth -> ServerT (Api Auth) ServerConfigReader
  server proxyEntity = update

userServerApi :: ServerT (Api User) ServerConfigReader
userServerApi =
  create :<|> delete (Proxy :: Proxy User) :<|> update :<|> list :<|> retrieve
