{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Examples.UsersWithBeamDB.Server
  ( runUsersWithBeamExample
  ) where

import Control.Monad.Trans.Reader
import Data.Proxy
import Network.Wai.Handler.Warp
import Servant

import Examples.UsersWithBeamDB.Model
import Examples.UsersWithBeamDB.RunDB
import Examples.UsersWithBeamDB.ServerConfig
import Examples.UsersWithBeamDB.UserEndpoint
import Resource

waiApplication :: Application
waiApplication request respond =
  initDB $ \conn ->
    let config = ServerConfig {port = 132, withDbConn = conn}
    in serve serverApi (initServer config) request respond

initServer :: ServerConfig -> Server (Api User)
initServer config = hoistServer serverApi transform fullServer
  where
    transform :: ServerConfigReader a -> Handler a
    transform handler = runReaderT (runServerConfigReader handler) config

fullServer :: ServerT (Api User) ServerConfigReader
fullServer = server (Proxy :: Proxy User)

serverApi :: Proxy (Api User)
serverApi = Proxy

runUsersWithBeamExample :: IO ()
runUsersWithBeamExample =
  putStrLn "Starting the web server..." >> run 8081 waiApplication
