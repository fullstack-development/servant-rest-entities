{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RestEntities.Examples.UsersWithBeamDB.ServerConfig where

import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except
import Control.Monad.Reader
import Database.Beam.Postgres (Pg)
import Servant
import Servant.Auth.Server

import RestEntities.DataProvider

data ServerConfig = ServerConfig
  { port :: Int
  , withDSConn :: forall a. Pg a -> IO a
  , jwtSettings :: JWTSettings
  , cookieSettings :: CookieSettings
  }

newtype ServerConfigReader a = ServerConfigReader
  { runServerConfigReader :: ReaderT ServerConfig Handler a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader ServerConfig
             , MonadError ServantErr
             , MonadThrow
             , MonadCatch
             )

instance HasDataSourceRun ServerConfigReader Pg where
  runDS action = do
    withConn <- asks withDSConn
    liftIO $ withConn action
