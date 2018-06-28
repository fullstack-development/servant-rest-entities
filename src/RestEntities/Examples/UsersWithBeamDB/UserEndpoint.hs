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

module RestEntities.Examples.UsersWithBeamDB.UserEndpoint where

import qualified Data.Aeson as Aeson
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import GHC.Generics
import Servant
import Servant.Auth.Server

import RestEntities.Examples.UsersWithBeamDB.DataSource ()
import RestEntities.Examples.UsersWithBeamDB.Model
import RestEntities.Examples.UsersWithBeamDB.ServerConfig
import RestEntities.Model
import RestEntities.Resource
import RestEntities.Routing
import RestEntities.Serializables
import RestEntities.WebActions.Create
import RestEntities.WebActions.Delete
import RestEntities.WebActions.List
import RestEntities.WebActions.Retrieve
import RestEntities.WebActions.Update

data AuthView = AuthView
  { authViewId :: Int
  , authViewPassword :: T.Text
  , authViewCreatedAt :: LocalTime
  } deriving (Generic, Aeson.ToJSON)

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

serializeAuthBody Auth {..} =
  AuthView
    { authViewId = fromId authId
    , authViewPassword = authPassword
    , authViewCreatedAt = authCreatedAt
    }

deserializeUserView Nothing UserBody {..} = do
  time <- getCurrentTime
  return
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
    , userViewAuth = serializeAuthBody userAuth
    }

instance ToJWT User

instance FromJWT User

instance Serializable User (CreateActionView User) where
  serialize user = CreateUserView $ serializeUserView user

instance Deserializable User (CreateActionBody User) where
  deserialize pk (CreateUserBody userBody) = deserializeUserView pk userBody

instance Serializable User (UpdateActionView User) where
  serialize user = UpdateUserView $ serializeUserView user

instance Deserializable User (UpdateActionBody User) where
  deserialize pk (UpdateUserBody userBody) = deserializeUserView pk userBody

instance Serializable User (ListActionView User) where
  serialize user = ListUserView $ serializeUserView user

instance Serializable User (RetrieveActionView User) where
  serialize user = RetrieveUserView $ serializeUserView user

instance HasCreateMethod User where
  type Requester User = User
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
  type Requester User = User
  data RetrieveActionView User = RetrieveUserView UserView
                                 deriving (Generic, Aeson.ToJSON)

instance Resource User where
  type Api User = CreateApi "users" (CreateActionBody User) (CreateActionView User) :<|> DeleteApi "users" :<|> UpdateApi "users" (UpdateActionBody User) (UpdateActionView User) :<|> ListApi "users" (ListActionView User) :<|> RetrieveApi "users" (RetrieveActionView User)
  server :: Proxy User -> ServerT (Api User) ServerConfigReader
  server proxyEntity = userServerApi

userServerApi :: ServerT (Api User) ServerConfigReader
userServerApi = create :<|> delete userProxy :<|> update :<|> list :<|> retrieve
  where
    userProxy = Proxy :: Proxy User
