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
{-# LANGUAGE UndecidableInstances #-}

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

instance Serializable User UserView where
  serialize = serializeUserView

instance Deserializable User UserBody where
  deserialize = deserializeUserView

instance HasCreateMethod User where
  type Requester User = User
  type CreateActionBody User = UserBody
  type CreateActionView User = UserView

instance HasUpdateMethod User where
  type UpdateActionBody User = UserBody
  type UpdateActionView User = UserView

instance HasDeleteMethod User

instance HasListMethod User where
  type ListActionView User = UserView

instance HasRetrieveMethod User where
  type Requester User = User
  type RetrieveActionView User = UserView

instance Resource User where
  type Api User = CreateApi "users" (CreateActionBody User) (CreateActionView User) :<|> DeleteApi "users" :<|> UpdateApi "users" (UpdateActionBody User) (UpdateActionView User) :<|> ListApi "users" (ListActionView User) :<|> RetrieveApi "users" (RetrieveActionView User)
  server :: Proxy User -> ServerT (Api User) ServerConfigReader
  server proxyEntity = userServerApi

userServerApi :: ServerT (Api User) ServerConfigReader
userServerApi = create :<|> delete userProxy :<|> update :<|> list :<|> retrieve
  where
    userProxy = Proxy :: Proxy User
