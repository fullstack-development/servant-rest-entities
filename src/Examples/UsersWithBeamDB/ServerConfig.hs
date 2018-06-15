{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Examples.UsersWithBeamDB.ServerConfig where

import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except
import Control.Monad.Reader
import DBEntity
import Database.Beam.Postgres (Pg)
import Servant

data ServerConfig = ServerConfig
  { port :: Int
  , withDbConn :: forall a. Pg a -> IO a
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

instance HasDbRun ServerConfigReader Pg where
  runDB action = do
    withConn <- asks withDbConn
    liftIO $ withConn action
