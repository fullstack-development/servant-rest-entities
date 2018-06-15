{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Examples.DB where

import DBEntity
import Data.Text
import Data.Time
import Data.Time.Clock
import qualified Model

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

fromColumn (Column v) = v

fromPK (PrimaryKey pk) = pk

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

def = error "Def db val"

instance DBEntity Model.User User where
  save user = pure undefined
  deleteFromDB _ _ = pure undefined
  getByIdFromDB _ = pure Nothing
  getByIdWithRelsFromDB _ _ = undefined
  getAllFromDB = pure []

instance DBEntity Model.Auth Auth where
  save user = pure undefined
  deleteFromDB _ _ = pure undefined
  getByIdFromDB _ = pure Nothing
  getByIdWithRelsFromDB _ _ = undefined
  getAllFromDB = pure []
