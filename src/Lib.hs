{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( runUserService
  ) where

import qualified DB
import Data.Proxy
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Handlers
import Model
import Routing
import Servant

defVal = error "Value is undefined"

type instance Relations User = Auth

instance Serializable User (View User) where
  serialize User {..} =
    UserView
    { userViewId = userId
    , userViewFirstName = userFirstName
    , userViewLastName = userLastName
    , userViewIsStaff = userIsStaff
    }

instance Deserializable User (Body User) where
  deserialize pk UserBody {..} = do
    time <- getCurrentTime
    return
      User
      { userId = fromMaybe defVal pk
      , userAuth =
          Auth
          { authId = defVal
          , authPassword = userBodyPassword
          , authCreatedAt = time
          }
      , userFirstName = userBodyFirstName
      , userLastName = userBodyLastName
      , userIsStaff = userBodyIsStaff
      , userCreatedAt = time
      }

instance DBEntity DB.User where
  save user = pure undefined
  deleteFromDB _ _ = pure undefined

instance DBConvertable User DB.User where
  dbConvertTo user rels = undefined
  dbConvertFrom dbUser = undefined

instance CRUDEntity User where
  data Body User = UserBody{userBodyFirstName :: T.Text,
                          userBodyLastName :: T.Text, userBodyIsStaff :: Bool,
                          userBodyPassword :: T.Text}
  data View User = UserView{userViewId :: Int,
                          userViewFirstName :: T.Text, userViewLastName :: T.Text,
                          userViewIsStaff :: Bool}
  type DBModel User = DB.User
  type Api User = CRUDApi "users" (Body User) (View User)
  server _ =
    retrieve :<|> list :<|> create :<|> delete (Proxy :: Proxy User) :<|> update

runUserService :: IO ()
runUserService = putStrLn "Running user service stub"
