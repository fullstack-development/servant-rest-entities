{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( runUserService
  ) where

import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import Servant

import qualified DB
import DBEntity
import Model
import Resource
import Routing
import Serializables
import WebActions.Create
import WebActions.Delete
import WebActions.Update

defVal = error "Value is undefined"

instance DBEntity DB.User where
  save user = pure undefined
  deleteFromDB _ _ = pure undefined

instance DBConvertable User DB.User where
  type DBModel User = DB.User
  type Relations User = Auth
  dbConvertTo user rels = undefined
  dbConvertFrom dbUser = undefined

data UserView = UserView
  { userViewId :: Int
  , userViewFirstName :: T.Text
  , userViewLastName :: T.Text
  , userViewIsStaff :: Bool
  }

data UserBody = UserBody
  { userBodyFirstName :: T.Text
  , userBodyLastName :: T.Text
  , userBodyIsStaff :: Bool
  , userBodyPassword :: T.Text
  }

deserializeUserBody pk UserBody {..} = do
  time <- getCurrentTime
  return
    User
    { userId = fromMaybe defVal pk
    , userAuth =
        Auth
        {authId = defVal, authPassword = userBodyPassword, authCreatedAt = time}
    , userFirstName = userBodyFirstName
    , userLastName = userBodyLastName
    , userIsStaff = userBodyIsStaff
    , userCreatedAt = time
    }

serializeUserBody User {..} =
  UserView
  { userViewId = userId
  , userViewFirstName = userFirstName
  , userViewLastName = userLastName
  , userViewIsStaff = userIsStaff
  }

instance Serializable User (CreateActionView User) where
  serialize user = CreateUserView $ serializeUserBody user

instance Deserializable User (CreateActionBody User) where
  deserialize pk (CreateUserBody userBody) = deserializeUserBody pk userBody

instance Serializable User (UpdateActionView User) where
  serialize user = UpdateUserView $ serializeUserBody user

instance Deserializable User (UpdateActionBody User) where
  deserialize pk (UpdateUserBody userBody) = deserializeUserBody pk userBody

instance HasCreateMethod User where
  data CreateActionBody User = CreateUserBody UserBody
  data CreateActionView User = CreateUserView UserView

instance HasUpdateMethod User where
  data UpdateActionBody User = UpdateUserBody UserBody
  data UpdateActionView User = UpdateUserView UserView

instance HasDeleteMethod User

instance Resource User where
  type Api User = CreateApi "users" (CreateActionBody User) (CreateActionView User) :<|> DeleteApi "users" :<|> UpdateApi "users" (UpdateActionBody User) (UpdateActionView User)
  server _ = create :<|> delete (Proxy :: Proxy User) :<|> update

runUserService :: IO ()
runUserService = putStrLn "Running user service stub"
