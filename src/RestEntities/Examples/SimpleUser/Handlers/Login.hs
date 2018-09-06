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

module RestEntities.Examples.SimpleUser.Handlers.Login
  ( login
  , LoginAPI
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as Char8
import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import qualified Data.Text as T
import GHC.Generics
import RestEntities.Examples.SimpleUser.DataSource ()
import RestEntities.Examples.SimpleUser.Model
import RestEntities.HasDataProvider.HasDataProvider
import Servant
import Servant.Auth.Server
import qualified Web.Cookie as Cookie

instance ToJWT User

instance FromJWT User

data LoginBody = LoginBody
  { username :: String
  , password :: String
  } deriving (Generic, Aeson.FromJSON, Show)

newtype LoginView = LoginView
  { token :: String
  } deriving (Generic, Aeson.FromJSON, Aeson.ToJSON)

type LoginResponse r
   = Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] r

type LoginAPI = "login" :> ReqBody '[ JSON] LoginBody :> Post '[ JSON] LoginView

login :: CookieSettings -> JWTSettings -> LoginBody -> Handler LoginView
login cookieSettings jwtSettings (LoginBody name password) = do
  users <- loadAll (Proxy :: Proxy User)
  resp <-
    runMaybeT $ do
      user <- MaybeT . return . L.find byNameAndPassword $ users
      let accept = acceptLogin cookieSettings jwtSettings user
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
