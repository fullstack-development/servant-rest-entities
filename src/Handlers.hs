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
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics
import Model
import Routing
import Servant
import Servant.API

type family Relations a

class Serializable e to | to -> e where
  serialize :: e -> to

class Deserializable e from | from -> e where
  deserialize :: Maybe Int -> from -> IO e

class (DBEntity to) =>
      DBConvertable e to
  | e -> to
  , to -> e
  where
  dbConvertTo :: e -> Maybe (Relations e) -> to
  dbConvertFrom :: to -> Maybe (Relations e) -> e

class DBEntity e where
  save :: e -> IO e
  deleteFromDB :: Proxy e -> Int -> IO (Either String ())

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
  create body = do
    model <- liftIO $ deserialize Nothing body
    let dbModel = dbConvertTo model Nothing
    newDbModel <- liftIO $ save dbModel
    let newModel = dbConvertFrom newDbModel Nothing
    let view = serialize newModel
    pure view
  update :: Int -> Body e -> Handler (View e)
  update entityId body = do
    model <- liftIO $ deserialize (Just entityId) body
    let dbModel = dbConvertTo model Nothing
    updatedDbModel <- liftIO $ save dbModel
    let updatedModel = dbConvertFrom updatedDbModel Nothing
    let view = serialize updatedModel
    pure view
  delete :: Proxy e -> Int -> Handler ()
  delete proxyType entityId = do
    result <- liftIO $ deleteFromDB (Proxy :: Proxy (DBModel e)) entityId
    case result of
      Left err -> throwError $ err400 {errBody = BL.pack err}
      Right () -> pure ()
  list :: Handler [View e]
  list = pure []
  retrieve :: Int -> Handler (View e)
  retrieve _ = pure undefined
