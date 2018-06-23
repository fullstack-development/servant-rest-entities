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
import qualified Data.List as L
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
import Examples.SimpleUser.Resources.UserResource
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

newtype LoginView = LoginView
  { token :: String
  } deriving (Generic, Aeson.FromJSON, Aeson.ToJSON)

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
      user <- MaybeT . return . L.find byNameAndPassword $ users
      let accept = ServantAuth.acceptLogin cookieSettings jwtSettings user
      mApplyCookies <- MaybeT . return <$> liftIO accept
      applyCookies :: NoContent -> LoginResponse NoContent <- mApplyCookies
      let cookiesResp = applyCookies NoContent
      let headers = getHeaders cookiesResp
      (_, tokenCookie) <- MaybeT . return . L.find findJwtHeader $ headers
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
