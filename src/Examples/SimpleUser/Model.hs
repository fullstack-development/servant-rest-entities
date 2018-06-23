{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Examples.SimpleUser.Model where

import Data.Aeson
import qualified Data.Text as T
import Data.Time.Clock
import GHC.Generics

import Model

data User = User
  { userId :: Id Int
  , userFirstName :: T.Text
  , userLastName :: T.Text
  , userCreatedAt :: UTCTime
  , userIsStaff :: Bool
  , userAuth :: Auth
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Auth = Auth
  { authId :: Id Int
  , authPassword :: T.Text
  , authCreatedAt :: UTCTime
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data BlogPost = BlogPost
  { postId :: Id Int
  , postText :: T.Text
  , postTitle :: T.Text
  , postAuthors :: [Author]
  }

data Author = Author
  { authorId :: Id Int
  , authorPseudonim :: T.Text
  , authorPosts :: [BlogPost]
  }
