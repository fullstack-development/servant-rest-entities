{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Examples.SimpleUser.DataSource where

import Control.Monad.Trans.Maybe
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Servant

import DataProvider
import qualified Examples.SimpleUser.Model as Model
import qualified Model

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
  , userFirstName :: Column T.Text
  , userLastName :: Column T.Text
  , userIsStaff :: Column Bool
  , userCreatedAt :: Column UTCTime
  } deriving (Show, Eq)

data Auth = Auth
  { authId :: PrimaryKey Int
  , authPassword :: Column T.Text
  , authCreatedAt :: Column UTCTime
  , authUserId :: ForeignKey (PrimaryKey Int)
  } deriving (Show, Eq)

data BlogPost = BlogPost
  { blogPostId :: PrimaryKey Int
  , blogPostText :: Column T.Text
  , blogPostTitle :: Column T.Text
  }

data Author = Author
  { authorId :: PrimaryKey Int
  , authorPseudonim :: Column T.Text
  }

data AuthorBlogPost = AuthorBlogPost
  { authorBlogPostId :: PrimaryKey Int
  , authorBlogPostPostId :: ForeignKey (PrimaryKey Int)
  , authorBlogPostAuthorId :: ForeignKey (PrimaryKey Int)
  }

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
      , userFirstName = Column "Sergey"
      , userLastName = Column "Cherepanov"
      , userIsStaff = Column True
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

authors =
  [ Author (PrimaryKey 1) (Column "tester 1")
  , Author (PrimaryKey 2) (Column "tester 2")
  , Author (PrimaryKey 3) (Column "tester 3")
  , Author (PrimaryKey 4) (Column "tester 4")
  ]

posts =
  [ BlogPost
      { blogPostId = PrimaryKey 1
      , blogPostText = Column "Text blog post with test content 1"
      , blogPostTitle = Column "Test blog post 1"
      }
  , BlogPost
      { blogPostId = PrimaryKey 2
      , blogPostText = Column "Text blog post with test content 2"
      , blogPostTitle = Column "Test blog post 2"
      }
  ]

postsAuthors
    -- First post
 =
  [ AuthorBlogPost
      (PrimaryKey 1)
      (ForeignKey $ PrimaryKey 1)
      (ForeignKey $ PrimaryKey 1)
  , AuthorBlogPost
      (PrimaryKey 2)
      (ForeignKey $ PrimaryKey 1)
      (ForeignKey $ PrimaryKey 2)
  , AuthorBlogPost
      (PrimaryKey 3)
      (ForeignKey $ PrimaryKey 1)
      (ForeignKey $ PrimaryKey 3)
      -- Second post
  , AuthorBlogPost
      (PrimaryKey 3)
      (ForeignKey $ PrimaryKey 2)
      (ForeignKey $ PrimaryKey 1)
  ]

instance HasDataProvider Model.User where
  type DataProviderModel Model.User = User
  type MonadDataProvider Model.User = Handler
  type ChildRelations Model.User = Model.Auth
  type ParentRelations Model.User = ()
  save = pure
  deleteById _ _ = pure $ Right ()
  loadById _ pk =
    runMaybeT $ do
      user <- wrap $ find (\u -> fromPK (userId u) == pk) users
      auth <- wrap $ find (\a -> fromFK (authUserId a) == userId user) auths
      let authModel = unpack auth (Just ())
      return $ unpack user (Just authModel)
    where
      wrap = MaybeT . return
  loadAll _ = pure $ map (\u -> unpack u (authFor u)) users
    where
      authFor user =
        flip unpack (Just ()) <$> find (authByUserId $ userId user) auths
      authByUserId id auth = fromFK (authUserId auth) == id
  unpack User {..} (Just auth) =
    Model.User
      { userId = Model.Id $ fromPK userId
      , userFirstName = fromColumn userFirstName
      , userLastName = fromColumn userLastName
      , userIsStaff = fromColumn userIsStaff
      , userCreatedAt = fromColumn userCreatedAt
      , userAuth = auth
      }
  unpack _ Nothing = error "You should pass all relations to user db converter."
  pack user@Model.User {..} _ = (providedUser, undefined)
    where
      auth = pack userAuth (Just user)
      providedUser =
        User
          { userId = PrimaryKey (Model.fromId userId)
          , userFirstName = Column userFirstName
          , userLastName = Column userLastName
          , userCreatedAt = Column userCreatedAt
          , userIsStaff = Column userIsStaff
          }

instance HasDataProvider Model.Auth where
  type DataProviderModel Model.Auth = Auth
  type ParentRelations Model.Auth = Model.User
  type ChildRelations Model.Auth = ()
  type MonadDataProvider Model.Auth = Handler
  save = pure
  deleteById _ _ = pure $ Right ()
  loadById _ pk =
    pure $ flip unpack Nothing <$> find (\a -> fromPK (authId a) == pk) auths
  loadAll _ = pure $ flip unpack Nothing <$> auths
  pack Model.Auth {..} (Just user) = (dbAuth, undefined)
    where
      dbAuth =
        Auth
          { authId =
              if Model.isIdEmpty authId
                then def
                else PrimaryKey (Model.fromId authId)
          , authPassword = Column authPassword
          , authCreatedAt = Column authCreatedAt
          , authUserId =
              ForeignKey (PrimaryKey (Model.fromId $ Model.userId user))
          }
  unpack Auth {..} _ =
    Model.Auth
      { authId = Model.Id $ fromPK authId
      , authPassword = fromColumn authPassword
      , authCreatedAt = fromColumn authCreatedAt
      }

instance HasDataProvider Model.BlogPost where
  type DataProviderModel Model.BlogPost = BlogPost
  type ParentRelations Model.BlogPost = ()
  type ChildRelations Model.BlogPost = [Model.Author]
  type MonadDataProvider Model.BlogPost = Handler
  unpack dpPost@BlogPost {..} (Just authors) =
    Model.BlogPost
      { Model.postId = Model.Id $ fromPK blogPostId
      , Model.postText = fromColumn blogPostText
      , Model.postTitle = fromColumn blogPostTitle
      , Model.postAuthors = authors
      }
  pack post@Model.BlogPost {..} _ = (dpPost, undefined)
    where
      dpPost =
        BlogPost
          { blogPostId = PrimaryKey $ Model.fromId postId
          , blogPostText = Column postText
          , blogPostTitle = Column postTitle
          }
      dpAuthors = map (flip pack (Just ())) postAuthors

instance HasDataProvider Model.Author where
  type DataProviderModel Model.Author = Author
  type ParentRelations Model.Author = ()
  type ChildRelations Model.Author = [Model.BlogPost]
  type MonadDataProvider Model.Author = Handler
