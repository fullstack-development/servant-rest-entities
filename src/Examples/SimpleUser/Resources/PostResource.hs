{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Examples.SimpleUser.Resources.PostResource where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import GHC.Generics

import Examples.SimpleUser.DataSource ()
import Examples.SimpleUser.Model
import Model
import Resource
import Routing
import Serializables
import WebActions.Retrieve

data PostView = PostView
  { postId :: Int
  , postText :: T.Text
  , postTitle :: T.Text
  , authors :: [AuthorView]
  } deriving (Generic, Aeson.ToJSON)

data AuthorView = AuthorView
  { authorId :: Int
  , authorName :: T.Text
  } deriving (Generic, Aeson.ToJSON)

instance Serializable LightAuthor AuthorView where
  serialize Author {..} =
    AuthorView {authorId = fromId authorId, authorName = authorPseudonim}

instance Serializable RichPost (RetrieveActionView RichPost) where
  serialize BlogPost {..} =
    RetrievePostView
      PostView
      { postId = fromId postId
      , postText = postText
      , postTitle = postTitle
      , authors = map serialize postAuthors
      }

instance HasRetrieveMethod RichPost where
  type Requester RichPost = RichPost
  data RetrieveActionView RichPost = RetrievePostView PostView
                                 deriving (Generic, Aeson.ToJSON)

instance Resource RichPost where
  type Api RichPost = RetrieveApi "posts" (RetrieveActionView RichPost)
  server p = retrieve
