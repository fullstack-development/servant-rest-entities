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

module Examples.UsersWithBeamDB.UserEndpoint where

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
import Servant.Auth.Server

import Data.Void
import qualified Examples.UsersWithBeamDB.DBEntity as DB
import qualified Examples.UsersWithBeamDB.Database as DB
import Examples.UsersWithBeamDB.Model
import qualified Examples.UsersWithBeamDB.RunDB as DB
import Examples.UsersWithBeamDB.ServerConfig
import Model
import Permissions
import Resource
import Routing
import Serializables
import WebActions.Create
import WebActions.Delete
import WebActions.List
import WebActions.Retrieve
import WebActions.Update

data UserView = UserView
  { userViewId :: Int
  , userViewFirstName :: T.Text
  , userViewLastName :: T.Text
  , userViewIsStaff :: Bool
  } deriving (Generic, Aeson.ToJSON)

data UserBody = UserBody
  { userBodyFirstName :: T.Text
  , userBodyLastName :: T.Text
  , userBodyIsStaff :: Bool
  , userBodyPassword :: T.Text
  } deriving (Generic, Aeson.FromJSON)

deserializeUserBody Nothing UserBody {..} = do
  time <- getCurrentTime
  return
    User
    { userId = Empty
    -- , userAuth =
    --     Auth
    --     {authId = Empty, authPassword = userBodyPassword, authCreatedAt = time}
    , userFirstName = userBodyFirstName
    , userLastName = userBodyLastName
    , userIsStaff = userBodyIsStaff
    , userCreatedAt = utcToLocalTime (minutesToTimeZone 0) time
    }

serializeUserBody User {..} =
  UserView
  { userViewId = fromId userId
  , userViewFirstName = userFirstName
  , userViewLastName = userLastName
  , userViewIsStaff = userIsStaff
  }

instance ToJWT User

instance FromJWT User

instance Serializable User (CreateActionView User) where
  serialize user = CreateUserView $ serializeUserBody user

instance Deserializable User (CreateActionBody User) where
  deserialize pk (CreateUserBody userBody) = deserializeUserBody pk userBody

instance Serializable User (UpdateActionView User) where
  serialize user = UpdateUserView $ serializeUserBody user

instance Deserializable User (UpdateActionBody User) where
  deserialize pk (UpdateUserBody userBody) = deserializeUserBody pk userBody

instance Serializable User (ListActionView User) where
  serialize user = ListUserView $ serializeUserBody user

instance Serializable User (RetrieveActionView User) where
  serialize user = RetrieveUserView $ serializeUserBody user

instance HasCreateMethod User where
  data CreateActionBody User = CreateUserBody UserBody
                           deriving (Generic, Aeson.FromJSON)
  data CreateActionView User = CreateUserView UserView
                           deriving (Generic, Aeson.ToJSON)

instance HasUpdateMethod User where
  data UpdateActionBody User = UpdateUserBody UserBody
                           deriving (Generic, Aeson.FromJSON)
  data UpdateActionView User = UpdateUserView UserView
                           deriving (Generic, Aeson.ToJSON)

instance HasDeleteMethod User

instance HasListMethod User where
  data ListActionView User = ListUserView UserView
                         deriving (Generic, Aeson.ToJSON)

instance HasRetrieveMethod User User where
  data RetrieveActionView User = RetrieveUserView UserView
                             deriving (Generic, Aeson.ToJSON)

instance Resource User where
  type Api User = CreateApi "users" (CreateActionBody User) (CreateActionView User) :<|> DeleteApi "users" :<|> UpdateApi "users" (UpdateActionBody User) (UpdateActionView User) :<|> ListApi "users" (ListActionView User) :<|> ProtectedApi '[ JWT] (RetrieveApi "users" (RetrieveActionView User)) User
  server :: Proxy User -> ServerT (Api User) ServerConfigReader
  server proxyEntity = userServerApi

userServerApi :: ServerT (Api User) ServerConfigReader
userServerApi =
  create :<|> delete userProxy :<|> update :<|> list :<|> retrieve' userProxy
  where
    userProxy = Proxy :: Proxy User
