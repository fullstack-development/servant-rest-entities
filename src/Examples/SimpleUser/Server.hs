{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Examples.SimpleUser.Server
  ( runUserService
  ) where

import qualified Data.Aeson as Aeson
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant

import DBEntity
import qualified Examples.SimpleUser.DB as DB
import Examples.SimpleUser.Model
import Model
import Resource
import Routing
import Serializables
import WebActions.Create
import WebActions.Delete
import WebActions.List
import WebActions.Retrieve
import WebActions.Update

type instance DBModel Auth = DB.Auth

type instance DBModel User = DB.User

instance DBConvertable Auth DB.Auth where
  type ChildRelations Auth = ()
  type ParentRelations Auth = User
  dbConvertTo Auth {..} (Just user) = (dbAuth, ())
    where
      dbAuth =
        DB.Auth
        { DB.authId =
            if isIdEmpty authId
              then DB.def
              else DB.PrimaryKey (fromId authId)
        , DB.authPassword = DB.Column authPassword
        , DB.authCreatedAt = DB.Column authCreatedAt
        , DB.authUserId = DB.ForeignKey (DB.PrimaryKey (fromId $ userId user))
        }
  dbConvertFrom DB.Auth {..} _ =
    Auth
    { authId = Id $ DB.fromPK authId
    , authPassword = DB.fromColumn authPassword
    , authCreatedAt = DB.fromColumn authCreatedAt
    }

instance DBConvertable User DB.User where
  type ChildRelations User = Auth
  type ParentRelations User = ()
  dbConvertTo user@User {..} _ = (dbUser, dbAuth)
    where
      (dbAuth, rels) = dbConvertTo userAuth (Just user)
      dbUser =
        DB.User
        { userId = DB.PrimaryKey (fromId userId)
        , userFirstName = DB.Column userFirstName
        , userLastName = DB.Column userLastName
        , userCreatedAt = DB.Column userCreatedAt
        , userIsStaff = DB.Column userIsStaff
        }
  dbConvertFrom DB.User {..} (Just auth) =
    User
    { userId = Id $ DB.fromPK userId
    , userFirstName = DB.fromColumn userFirstName
    , userLastName = DB.fromColumn userLastName
    , userIsStaff = DB.fromColumn userIsStaff
    , userCreatedAt = DB.fromColumn userCreatedAt
    , userAuth = dbConvertFrom auth Nothing
    }
  dbConvertFrom _ Nothing =
    error "You should pass all relations to user db converter."

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
    , userAuth =
        Auth
        {authId = Empty, authPassword = userBodyPassword, authCreatedAt = time}
    , userFirstName = userBodyFirstName
    , userLastName = userBodyLastName
    , userIsStaff = userBodyIsStaff
    , userCreatedAt = time
    }

serializeUserBody User {..} =
  UserView
  { userViewId = fromId userId
  , userViewFirstName = userFirstName
  , userViewLastName = userLastName
  , userViewIsStaff = userIsStaff
  }

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

instance HasRetrieveMethod User where
  data RetrieveActionView User = RetrieveUserView UserView
                             deriving (Generic, Aeson.ToJSON)

instance Resource User where
  type Api User = CreateApi "users" (CreateActionBody User) (CreateActionView User) :<|> DeleteApi "users" :<|> UpdateApi "users" (UpdateActionBody User) (UpdateActionView User) :<|> ListApi "users" (ListActionView User) :<|> RetrieveApi "users" (RetrieveActionView User)
  server proxyEntity =
    create :<|> delete proxyEntity :<|> update :<|> list :<|> retrieve

fullServer = server (Proxy :: Proxy User)

serverApi :: Proxy (Api User)
serverApi = Proxy

serverApp :: Application
serverApp = serve serverApi fullServer

runUserService :: IO ()
runUserService = run 8081 serverApp
