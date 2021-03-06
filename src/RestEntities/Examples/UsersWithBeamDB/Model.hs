{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module RestEntities.Examples.UsersWithBeamDB.Model where

import Data.Aeson
import qualified Data.Text as T
import Data.Time.LocalTime
import GHC.Generics

import RestEntities.Model

data User = User
  { userId :: Id Int
  , userFirstName :: T.Text
  , userLastName :: T.Text
  , userCreatedAt :: LocalTime
  , userIsStaff :: Bool
  , userAuth :: Auth
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Auth = Auth
  { authId :: Id Int
  , authPassword :: T.Text
  , authCreatedAt :: LocalTime
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)
