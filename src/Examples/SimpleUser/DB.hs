{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Examples.SimpleUser.DB where

import Data.List
import Data.Text hiding (find)
import Data.Time
import Data.Time.Clock
import Servant

import DBEntity
import qualified Examples.SimpleUser.Model as Model

def = error "Default does not exist"

fromColumn (Column v) = v

fromPK (PrimaryKey pk) = pk

fromFK (ForeignKey k) = k

newtype PrimaryKey a =
  PrimaryKey a
  deriving (Show, Eq)

newtype ForeignKey a =
  ForeignKey a
  deriving (Show, Eq)

newtype Column a =
  Column a
  deriving (Show, Eq)

data User = User
  { userId :: PrimaryKey Int
  , userFirstName :: Column Text
  , userLastName :: Column Text
  , userIsStaff :: Column Bool
  , userCreatedAt :: Column UTCTime
  } deriving (Show, Eq)

data Auth = Auth
  { authId :: PrimaryKey Int
  , authPassword :: Column Text
  , authCreatedAt :: Column UTCTime
  , authUserId :: ForeignKey (PrimaryKey Int)
  } deriving (Show, Eq)

time = getCurrentTime

users =
  [ User
    { userId = PrimaryKey 1
    , userFirstName = Column "Nikita"
    , userLastName = Column "Razmakhnin"
    , userIsStaff = Column False
    , userCreatedAt = Column $ UTCTime (ModifiedJulianDay 0) 0
    }
  , User
    { userId = PrimaryKey 2
    , userFirstName = Column "Nikita"
    , userLastName = Column "Razmakhnin"
    , userIsStaff = Column False
    , userCreatedAt = Column $ UTCTime (ModifiedJulianDay 0) 0
    }
  ]

auths =
  [ Auth
    { authId = PrimaryKey 1
    , authPassword = Column "test test"
    , authCreatedAt = Column $ UTCTime (ModifiedJulianDay 0) 0
    , authUserId = ForeignKey (PrimaryKey 1)
    }
  , Auth
    { authId = PrimaryKey 2
    , authPassword = Column "test test"
    , authCreatedAt = Column $ UTCTime (ModifiedJulianDay 0) 0
    , authUserId = ForeignKey (PrimaryKey 2)
    }
  ]

type instance DBModel Model.Auth = Auth

type instance DBModel Model.User = User

instance DBEntity Model.User User where
  type MonadDB User = Handler
  type ChildRelations Model.User = Model.Auth
  type ParentRelations Model.User = ()
  save user = pure undefined
  deleteFromDB _ _ = pure undefined
  getByIdFromDB pk = pure Nothing
  getByIdWithRelsFromDB pk _ = do
    let user = find (\u -> fromPK (userId u) == pk) users
    let auth =
          maybe
            Nothing
            (\u -> find (\a -> fromFK (authUserId a) == userId u) auths)
            user
    case (user, auth) of
      (Just u, Just a) -> return $ Just (u, a)
      _ -> return Nothing
  getAllFromDBWithRels = undefined
  getAllFromDB = pure []

instance DBEntity Model.Auth Auth where
  type MonadDB Auth = Handler
  type ChildRelations Model.Auth = ()
  type ParentRelations Model.Auth = Model.User
  save user = pure undefined
  deleteFromDB _ _ = pure undefined
  getByIdFromDB _ = pure Nothing
  getByIdWithRelsFromDB _ _ = undefined
  getAllFromDBWithRels = undefined
  getAllFromDB = pure []
