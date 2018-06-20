{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Examples.SimpleUser.Server
  ( runUserService
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Data.Aeson as Aeson
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Data.Time.Clock
import Data.Void
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import qualified Servant.Auth.Server as ServantAuth

import DBEntity
import qualified Examples.SimpleUser.DB as DB
import Examples.SimpleUser.DBBridge
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
  type Requester User = User
  data RetrieveActionView User = RetrieveUserView UserView
                             deriving (Generic, Aeson.ToJSON)
  checkEntityPermission (Just user) entity =
    return (userId user == userId entity)
  checkEntityPermission _ _ = return False

instance Resource User where
  type Api User = CreateApi "users" (CreateActionBody User) (CreateActionView User) :<|> DeleteApi "users" :<|> UpdateApi "users" (UpdateActionBody User) (UpdateActionView User) :<|> ListApi "users" (ListActionView User) :<|> ProtectedApi '[ ServantAuth.JWT] (RetrieveApi "users" (RetrieveActionView User)) User
  server proxyEntity =
    create :<|> delete proxyEntity :<|> update :<|> list :<|>
    retrieve' proxyEntity

fullApi cs jwts = server (Proxy :: Proxy User) :<|> login cs jwts

type LoginAPI
   = "login" :> ReqBody '[ JSON] LoginBody :> PostNoContent '[ JSON] LoginResponse

type LoginResponse
   = Headers '[ Header "Set-Cookie" ServantAuth.SetCookie, Header "Set-Cookie" ServantAuth.SetCookie] NoContent

type FullApi = Api User :<|> LoginAPI

routes :: Proxy FullApi
routes = Proxy

mkApp jwtSecret = do
  time <- getCurrentTime
  let authDuration = fromRational 1 :: NominalDiffTime
  let authExpiresIn = addUTCTime authDuration time
  let cookieCfg =
        ServantAuth.defaultCookieSettings
        {ServantAuth.cookieExpires = Just authExpiresIn}
  let jwtCfg = ServantAuth.defaultJWTSettings jwtSecret
  let cfg = cookieCfg :. jwtCfg :. EmptyContext
  return $ serveWithContext routes cfg (fullApi cookieCfg jwtCfg)

runUserService :: IO ()
runUserService = do
  jwtSecret <- ServantAuth.generateKey
  app <- mkApp jwtSecret
  run 8081 app

instance ServantAuth.ToJWT User

instance ServantAuth.FromJWT User

data LoginBody = LoginBody
  { username :: String
  , password :: String
  } deriving (Generic, Aeson.FromJSON, Show)

login ::
     ServantAuth.CookieSettings
  -> ServantAuth.JWTSettings
  -> LoginBody
  -> Handler LoginResponse
login cookieSettings jwtSettings (LoginBody name password) = do
  mDbUser <- getByIdWithRelsFromDB 1 (Proxy :: Proxy DB.User)
  resp <-
    runMaybeT $ do
      (model, rels) <- MaybeT . return $ mDbUser
      let user = dbConvertFrom model (Just rels)
      let accept = ServantAuth.acceptLogin cookieSettings jwtSettings user
      mApplyCookies <- liftIO accept
      applyCookies <- MaybeT . return $ mApplyCookies
      return $ applyCookies NoContent
  when (isNothing resp) (throwError err401)
  return . fromJust $ resp
