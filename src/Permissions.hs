module Permissions where

import DBEntity
import Data.Proxy

type AccessPermissionCheck e requester
   = Maybe requester -> Proxy e -> MonadDB (DBModel e) Bool

type EntityPermissionCheck e requester
   = Maybe requester -> e -> MonadDB (DBModel e) Bool
