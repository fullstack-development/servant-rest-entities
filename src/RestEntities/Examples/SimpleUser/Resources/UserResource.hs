{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Examples.SimpleUser.Resources.UserResource
  (
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Data.Time
import Examples.SimpleUser.DataSource ()
import GHC.Generics
import Servant
import qualified Servant.Auth as ServantAuth

import Examples.SimpleUser.Model
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

deserializeUserView Nothing UserBody {..} = do
  time <- getCurrentTime
  return
    User
      { userId = Empty
      , userAuth =
          Auth
            { authId = Empty
            , authPassword = userBodyPassword
            , authCreatedAt = time
            }
      , userFirstName = userBodyFirstName
      , userLastName = userBodyLastName
      , userIsStaff = userBodyIsStaff
      , userCreatedAt = time
      }

serializeUserView User {..} =
  UserView
    { userViewId = fromId userId
    , userViewFirstName = userFirstName
    , userViewLastName = userLastName
    , userViewIsStaff = userIsStaff
    }

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
  checkEntityPermission (Just user) entity =
    return (userId user == userId entity)
  checkEntityPermission _ _ = return False

type Create = CreateApi "users" (CreateActionBody User) (CreateActionView User)

type Update = UpdateApi "users" (UpdateActionBody User) (UpdateActionView User)

type Del = DeleteApi "users"

type List = ListApi "users" (ListActionView User)

type Retrieve
   = ProtectedApi '[ ServantAuth.JWT] (RetrieveApi "users" (RetrieveActionView User)) User

instance Resource User where
  type Api User = Create :<|> Del :<|> Update :<|> List :<|> Retrieve
  server p = create :<|> delete p :<|> update :<|> list :<|> retrieve'
