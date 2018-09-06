{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module RestEntities.Examples.SimpleUser.Resources.PostResource
  (
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import GHC.Generics

import RestEntities.Examples.SimpleUser.DataSource ()
import RestEntities.Examples.SimpleUser.Model
import RestEntities.Model
import RestEntities.Serializables

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
