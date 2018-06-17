{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Examples.SimpleUser.Server
  ( runUserService
  ) where

import qualified Data.Aeson as Aeson
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant

import DBEntity
import qualified Examples.SimpleUser.DB as DB
import Examples.SimpleUser.Model
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
  type MonadWeb User = Handler
  server proxyEntity =
    create :<|> delete proxyEntity :<|> update :<|> list :<|> retrieve

instance Resource Auth where
  type Api Auth = UpdateApi "auth" (UpdateActionBody Auth) (UpdateActionView Auth)
  type MonadWeb Auth = Handler
  server proxyEntity = update

fullServer = server (Proxy :: Proxy User)

serverApi :: Proxy (Api User)
serverApi = Proxy

serverApp :: Application
serverApp = serve serverApi fullServer

runUserService :: IO ()
runUserService = run 8081 serverApp
