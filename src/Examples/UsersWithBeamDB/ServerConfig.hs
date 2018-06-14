{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Examples.UsersWithBeamDB.ServerConfig where

import Control.Monad.Catch hiding (Handler)
import Control.Monad.Except
import Control.Monad.Reader
import Servant

data ServerConfig = ServerConfig
  { port :: Int
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
