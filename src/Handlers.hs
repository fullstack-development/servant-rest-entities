{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Handlers where

import Control.Monad.IO.Class
import Data.Aeson
import GHC.Generics hiding ((:+:))
import Model
import Routing
import Servant
import Servant.API

data (a :+: b) where
  (:+:) :: a -> b -> (a :+: b)

data Relation a
  = Relation a
  | Nil

type family Relations a where
  Relations User = Auth

class Serializable e to where
  serialize :: e -> to

class Deserializable e from where
  deserialize :: Maybe Int -> from -> IO e

class (DBEntity to) =>
      DBConvertable e to
  | e -> to
  , to -> e
  where
  dbConvertTo :: e -> Relations e -> to
  dbConvertFrom :: to -> Relations e -> e

class DBEntity e where
  save :: e -> IO e

type CRUDConstraint e
   = ( Generic e
     , Deserializable e (Body e)
     , Serializable e (View e)
     , DBConvertable e (DBModel e))

class CRUDConstraint e =>
      CRUDEntity e
  | e -> e
  where
  data Body e
  data View e
  type DBModel e
  type Api e
  -- Example of instantiate:
  --    userServer = server (Proxy :: Proxy User)
  server :: Proxy e -> Server (Api e)
  create :: Body e -> Handler (View e)
  -- create body = do
  --   model <- liftIO (deserialize Nothing body :: IO e)
  --   let dbModel = dbConvertTo model :: DBModel e
  --   newDbModel <- liftIO $ save dbModel
  --   let newModel = dbConvertFrom newDbModel :: e
  --   let view = serialize newModel :: View e
  --   return view
  update :: Body e -> entity -> Handler (View e)
  update _ _ = return undefined
  delete :: Integer -> Handler e
  delete _ = return undefined
  list :: Handler [View e]
  list = return []
  retrieve :: Integer -> Handler (View e)
  retrieve _ = return undefined
