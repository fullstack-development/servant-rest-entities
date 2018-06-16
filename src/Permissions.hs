module Permissions where

import DBEntity
import Data.Proxy

type AccessPermissionCheck e = Proxy e -> MonadDB (DBModel e) Bool

type EntityPermissionCheck e = e -> MonadDB (DBModel e) Bool
