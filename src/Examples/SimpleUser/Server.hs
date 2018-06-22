{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as Char8
import qualified Data.CaseInsensitive as CI
import Data.List hiding (delete)
import Data.Proxy
import qualified Data.Text as T
import Data.Time.Clock
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import qualified Servant.Auth.Server as ServantAuth
import qualified Web.Cookie as Cookie

import DataProvider
import qualified Examples.SimpleUser.DataSource as DS
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

newtype LoginView = LoginView
  { token :: String
  } deriving (Generic, Aeson.FromJSON, Aeson.ToJSON)

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

instance Resource User where
  type Api User = CreateApi "users" (CreateActionBody User) (CreateActionView User) :<|> DeleteApi "users" :<|> UpdateApi "users" (UpdateActionBody User) (UpdateActionView User) :<|> ListApi "users" (ListActionView User) :<|> ProtectedApi '[ ServantAuth.JWT] (RetrieveApi "users" (RetrieveActionView User)) User
  server proxyEntity =
    create :<|> delete proxyEntity :<|> update :<|> list :<|> retrieve'

fullApi cs jwts = server (Proxy :: Proxy User) :<|> login cs jwts

type LoginAPI = "login" :> ReqBody '[ JSON] LoginBody :> Post '[ JSON] LoginView

type LoginResponse r
   = Headers '[ Header "Set-Cookie" ServantAuth.SetCookie, Header "Set-Cookie" ServantAuth.SetCookie] r

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
  -> Handler LoginView
login cookieSettings jwtSettings (LoginBody name password) = do
  users <- loadAll (Proxy :: Proxy User)
  resp <-
    runMaybeT $ do
      user <- MaybeT . return . find byNameAndPassword $ users
      let accept = ServantAuth.acceptLogin cookieSettings jwtSettings user
      mApplyCookies <- MaybeT . return <$> liftIO accept
      applyCookies :: NoContent -> LoginResponse NoContent <- mApplyCookies
      let cookiesResp = applyCookies NoContent
      let headers = getHeaders cookiesResp
      (_, tokenCookie) <- MaybeT . return . find findJwtHeader $ headers
      let jwtToken =
            Char8.unpack . Cookie.setCookieValue . Cookie.parseSetCookie $
            tokenCookie
      return $ LoginView jwtToken
  maybe (throwError err401) return resp
  where
    byNameAndPassword User {..} =
      userFirstName == T.pack name && (authPassword userAuth == T.pack password)
    findJwtHeader (key, _)
      | CI.mk "Set-Cookie" == key = True
      | otherwise = False
