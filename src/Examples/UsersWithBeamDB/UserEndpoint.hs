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

import qualified Examples.UsersWithBeamDB.DBEntity as DB
import qualified Examples.UsersWithBeamDB.Database as DB
import Examples.UsersWithBeamDB.Model
import qualified Examples.UsersWithBeamDB.RunDB as DB
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

data AuthView = AuthView
  { authViewId :: Int
  , authViewPassword :: T.Text
  , authViewCreatedAt :: LocalTime
  } deriving (Generic, Aeson.ToJSON)

data AuthBody = AuthBody
  { authBodyPassword :: T.Text
  } deriving (Generic, Aeson.FromJSON)

data UserView = UserView
  { userViewId :: Int
  , userViewFirstName :: T.Text
  , userViewLastName :: T.Text
  , userViewIsStaff :: Bool
  , userViewAuth :: AuthView
  } deriving (Generic, Aeson.ToJSON)

data UserBody = UserBody
  { userBodyFirstName :: T.Text
  , userBodyLastName :: T.Text
  , userBodyIsStaff :: Bool
  , userBodyPassword :: T.Text
  } deriving (Generic, Aeson.FromJSON)

serializeAuthView Auth {..} =
  AuthView
  { authViewId = fromId authId
  , authViewPassword = authPassword
  , authViewCreatedAt = authCreatedAt
  }

deserializeAuthBody Nothing AuthBody {..} = do
  time <- getCurrentTime
  pure
    Auth
    { authId = Empty
    , authPassword = authBodyPassword
    , authCreatedAt = utcToLocalTime (minutesToTimeZone 0) time
    }

deserializeUserBody Nothing UserBody {..} = do
  time <- getCurrentTime
  pure
    User
    { userId = Empty
    , userAuth =
        Auth
        { authId = Empty
        , authPassword = userBodyPassword
        , authCreatedAt = utcToLocalTime (minutesToTimeZone 0) time
        }
    , userFirstName = userBodyFirstName
    , userLastName = userBodyLastName
    , userIsStaff = userBodyIsStaff
    , userCreatedAt = utcToLocalTime (minutesToTimeZone 0) time
    }

serializeUserView User {..} =
  UserView
  { userViewId = fromId userId
  , userViewFirstName = userFirstName
  , userViewLastName = userLastName
  , userViewIsStaff = userIsStaff
  , userViewAuth = serializeAuthView userAuth
  }

instance Serializable Auth (UpdateActionView Auth) where
  serialize auth = UpdateAuthView $ serializeAuthView auth

instance Deserializable Auth (UpdateActionBody Auth) where
  deserialize pk (UpdateAuthBody authBody) = deserializeAuthBody pk authBody

instance Serializable User (CreateActionView User) where
  serialize user = CreateUserView $ serializeUserView user

instance Deserializable User (CreateActionBody User) where
  deserialize pk (CreateUserBody userBody) = deserializeUserBody pk userBody

instance Serializable User (UpdateActionView User) where
  serialize user = UpdateUserView $ serializeUserView user

instance Deserializable User (UpdateActionBody User) where
  deserialize pk (UpdateUserBody userBody) = deserializeUserBody pk userBody

instance Serializable User (ListActionView User) where
  serialize user = ListUserView $ serializeUserView user

instance Serializable User (RetrieveActionView User) where
  serialize user = RetrieveUserView $ serializeUserView user

instance HasUpdateMethod Auth where
  data UpdateActionBody Auth = UpdateAuthBody AuthBody
                           deriving (Generic, Aeson.FromJSON)
  data UpdateActionView Auth = UpdateAuthView AuthView
                           deriving (Generic, Aeson.ToJSON)

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

instance HasRetrieveMethod User where
  data RetrieveActionView User = RetrieveUserView UserView
                             deriving (Generic, Aeson.ToJSON)

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
