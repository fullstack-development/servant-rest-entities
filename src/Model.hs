{-# LANGUAGE DeriveGeneric #-}

module Model where

import qualified Data.Text as T
import Data.Time.Clock
import GHC.Generics

data User = User
  { userId :: Int
  , userFirstName :: T.Text
  , userLastName :: T.Text
  , userCreatedAt :: UTCTime
  , userIsStaff :: Bool
  , userAuth :: Auth
  } deriving (Show, Eq, Generic)

data Auth = Auth
  { authId :: Int
  , authPassword :: T.Text
  , authCreatedAt :: UTCTime
  } deriving (Show, Eq, Generic)
