{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module RestEntities.Examples.UsersWithBeamDB.Server
  ( runUsersWithBeamExample
  ) where

import Control.Monad.Trans.Reader
import Data.Proxy
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server

import RestEntities.Examples.UsersWithBeamDB.Model
import RestEntities.Examples.UsersWithBeamDB.RunDB
import RestEntities.Examples.UsersWithBeamDB.ServerConfig
import RestEntities.Examples.UsersWithBeamDB.UserEndpoint ()
import RestEntities.Resource

type FullApi = Api User

waiApplication :: Application
waiApplication request respond = do
  jwtSecret <- generateKey
  initDB $ \conn ->
    let cookieSettings = defaultCookieSettings
        jwtSettings = defaultJWTSettings jwtSecret
        serverConfig =
          ServerConfig
            { port = 132
            , withDSConn = conn
            , jwtSettings = jwtSettings
            , cookieSettings = cookieSettings
            }
        cfg = serverConfig :. cookieSettings :. jwtSettings :. EmptyContext
     in serveWithContext serverApi cfg (initServer serverConfig) request respond

initServer :: ServerConfig -> Server FullApi
initServer config =
  hoistServerWithContext serverApi contextProxy transform fullServer
  where
    contextProxy = Proxy :: Proxy '[ ServerConfig, CookieSettings, JWTSettings]
    transform :: ServerConfigReader a -> Handler a
    transform handler = runReaderT (runServerConfigReader handler) config

fullServer :: ServerT FullApi ServerConfigReader
fullServer = server (Proxy :: Proxy User)

serverApi :: Proxy FullApi
serverApi = Proxy

runUsersWithBeamExample :: IO ()
runUsersWithBeamExample =
  putStrLn "Starting the web server..." >> run 8081 waiApplication
