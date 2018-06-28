{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module RestEntities.Examples.SimpleUser.Model
  ( User(..)
  , Auth(..)
  , BlogPost(..)
  , Author(..)
  , RichAuthor
  , RichPost
  , LightAuthor
  , LightPost
  ) where

import Data.Aeson
import qualified Data.Text as T
import Data.Time.Clock
import GHC.Generics

import RestEntities.Model

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

data BlogPost authors = BlogPost
  { postId :: Id Int
  , postText :: T.Text
  , postTitle :: T.Text
  , postAuthors :: authors
  } deriving (Generic)

data Author posts = Author
  { authorId :: Id Int
  , authorPseudonim :: T.Text
  , authorPosts :: posts
  } deriving (Generic)

type RichAuthor = Author [LightPost]

type RichPost = BlogPost [LightAuthor]

type LightAuthor = Author NotRich

type LightPost = BlogPost NotRich
