{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RestEntities.Examples.SimpleUser.Resources.UserResource
  (
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Data.Time
import GHC.Generics
import Servant
import qualified Servant.Auth as ServantAuth

import RestEntities.Examples.SimpleUser.DataSource ()
import RestEntities.Examples.SimpleUser.Model
import RestEntities.Model
import RestEntities.Permissions
import RestEntities.Resource
import RestEntities.Routing
import RestEntities.Serializables
import RestEntities.WebActions.Create
import RestEntities.WebActions.Delete
import RestEntities.WebActions.List
import RestEntities.WebActions.Retrieve
import RestEntities.WebActions.Update

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
