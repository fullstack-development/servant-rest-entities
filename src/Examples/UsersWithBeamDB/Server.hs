{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Examples.UsersWithBeamDB.Server
  ( runUserService
  ) where

import Control.Monad.Trans.Reader
import Data.Proxy
import Network.Wai.Handler.Warp
import Servant

import Examples.UsersWithBeamDB.Model
import Examples.UsersWithBeamDB.ServerConfig
import Examples.UsersWithBeamDB.UserEndpoint
import Resource

createApp :: ServerConfig -> Application
createApp config = serve serverApi $ initBotServer config

initBotServer :: ServerConfig -> Server (Api User)
initBotServer config = hoistServer serverApi transform fullServer
  where
    transform :: ServerConfigReader a -> Handler a
    transform handler = runReaderT (runServerConfigReader handler) config

mkApp :: IO Application
mkApp = do
  putStrLn "Starting the web server..."
  let config = ServerConfig {port = 132}
  return $ createApp config

fullServer :: ServerT (Api User) ServerConfigReader
fullServer = server (Proxy :: Proxy User)

serverApi :: Proxy (Api User)
serverApi = Proxy

runUserService :: IO ()
runUserService = mkApp >>= run 8081
