{-# LANGUAGE DeriveGeneric #-}

module Model where

import qualified Data.Text as T
import Data.Time.Clock
import GHC.Generics

data Id a
  = Empty
  | Id a
  deriving (Show, Eq, Generic)

fromId (Id v) = v
fromId Empty = error "Could not unpack empty id of model"

isIdEmpty Empty = True
isIdEmpty _ = False

data User = User
  { userId :: Id Int
  , userFirstName :: T.Text
  , userLastName :: T.Text
  , userCreatedAt :: UTCTime
  , userIsStaff :: Bool
  , userAuth :: Auth
  } deriving (Show, Eq, Generic)

data Auth = Auth
  { authId :: Id Int
  , authPassword :: T.Text
  , authCreatedAt :: UTCTime
  } deriving (Show, Eq, Generic)
