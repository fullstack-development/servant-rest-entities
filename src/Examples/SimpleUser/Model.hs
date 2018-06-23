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

type AuthorWithPosts = Author [NotRichPost]

type RichPost = BlogPost [LightAuthor]

type LightAuthor = Author NotRich

type NotRichPost = BlogPost NotRich
