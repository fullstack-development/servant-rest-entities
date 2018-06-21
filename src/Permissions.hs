{-# LANGUAGE TypeOperators #-}

module Permissions where

import DBEntity
import Data.Proxy
import Servant
import Servant.Auth.Server

type AccessPermissionCheck e requester
   = Maybe requester -> Proxy e -> MonadDB (DBModel e) Bool

type EntityPermissionCheck e requester
   = Maybe requester -> e -> MonadDB (DBModel e) Bool

type ProtectedApi auths api requester = Auth auths requester :> api
