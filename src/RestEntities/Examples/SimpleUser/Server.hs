{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RestEntities.Examples.SimpleUser.Server
  ( runUserService
  ) where

import Data.Proxy
import Data.Time.Clock
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server

import RestEntities.Examples.SimpleUser.Handlers.Login (LoginAPI, login)
import RestEntities.Examples.SimpleUser.Model
import RestEntities.Examples.SimpleUser.Resources.PostResource ()
import RestEntities.Examples.SimpleUser.Resources.UserResource ()
import RestEntities.Resource

fullApi cs jwts =
  server (Proxy :: Proxy User) :<|> login cs jwts :<|>
  server (Proxy :: Proxy RichPost)

type FullApi = Api User :<|> LoginAPI :<|> Api RichPost

routes :: Proxy FullApi
routes = Proxy

mkApp jwtSecret = do
  time <- getCurrentTime
  let authDuration = fromRational 1 :: NominalDiffTime
  let authExpiresIn = addUTCTime authDuration time
  let cookieCfg = defaultCookieSettings {cookieExpires = Just authExpiresIn}
  let jwtCfg = defaultJWTSettings jwtSecret
  let cfg = cookieCfg :. jwtCfg :. EmptyContext
  return $ serveWithContext routes cfg (fullApi cookieCfg jwtCfg)

runUserService :: IO ()
runUserService = do
  jwtSecret <- generateKey
  app <- mkApp jwtSecret
  run 8081 app
