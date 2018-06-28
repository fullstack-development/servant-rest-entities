{-# LANGUAGE TypeOperators #-}

module RestEntities.Permissions where

import Data.Proxy
import Servant
import Servant.Auth.Server

type AccessPermissionCheck requester m entity
   = Proxy entity -> Maybe requester -> m Bool

type EntityPermissionCheck requester m entity
   = Maybe requester -> entity -> m Bool

type ProtectedApi auths api requester = Auth auths requester :> api
