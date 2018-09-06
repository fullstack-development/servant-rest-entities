{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module RestEntities.Examples.SimpleUser.DataSource
  (
  ) where

import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Servant

import RestEntities.DataProvider
import qualified RestEntities.Examples.SimpleUser.Model as Model
import RestEntities.HasDataProvider.HasDataProvider
import qualified RestEntities.Model as Model

def = error "Default does not exist"

fromColumn (Column v) = v

newtype PrimaryKey a = PrimaryKey
  { fromPK :: a
  } deriving (Show, Eq)

newtype ForeignKey a = ForeignKey
  { fromFK :: a
  } deriving (Show, Eq)

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
  type ChildRelations Model.User = SingleChild Model.Auth
  type ParentRelations Model.User = ()
  getPK _ = fromPK . userId
  getID = Model.userId
  unpack User {..} (auth, _) =
    Model.User
      { userId = Model.Id $ fromPK userId
      , userFirstName = fromColumn userFirstName
      , userLastName = fromColumn userLastName
      , userIsStaff = fromColumn userIsStaff
      , userCreatedAt = fromColumn userCreatedAt
      , userAuth = unpack auth ()
      }
  pack user@Model.User {..} _ = (providedUser, auth)
    where
      auth = pack userAuth (user, ())
      providedUser =
        User
          { userId = PrimaryKey (Model.fromId userId)
          , userFirstName = Column userFirstName
          , userLastName = Column userLastName
          , userCreatedAt = Column userCreatedAt
          , userIsStaff = Column userIsStaff
          }

instance HasSaveableDataProvider Model.User where
  save model _ = pure model

instance HasDeleteableDataProvider Model.User where
  deleteById _ _ = pure $ Right ()

instance HasLoadableDataProvider Model.User where
  loadById _ pk =
    runMaybeT $ do
      user <- wrap $ find (\u -> fromPK (userId u) == pk) users
      auth <- wrap $ find (\a -> fromFK (authUserId a) == userId user) auths
      return $ unpack user (auth, ())
    where
      wrap = MaybeT . return
  loadAll _ = pure $ map (\user -> unpack user (authFor user, ())) users
    where
      authFor user = fromJust $ find (authByUserId $ userId user) auths
      authByUserId id auth = fromFK (authUserId auth) == id

instance HasDataProvider Model.Auth where
  type DataProviderModel Model.Auth = Auth
  type ParentRelations Model.Auth = Model.User
  type ChildRelations Model.Auth = EmptyChild
  type MonadDataProvider Model.Auth = Handler
  getPK _ = fromPK . authId
  getID = Model.authId
  pack Model.Auth {..} (user, _) = (dbAuth, ())
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

instance HasDataProvider Model.RichPost where
  type DataProviderModel Model.RichPost = BlogPost
  type ParentRelations Model.RichPost = ()
  type ChildRelations Model.RichPost = ManyChildren Model.LightAuthor
  type MonadDataProvider Model.RichPost = Handler
  getPK _ = fromPK . blogPostId
  getID = Model.postId
  unpack BlogPost {..} authors =
    Model.BlogPost
      { Model.postId = Model.Id $ fromPK blogPostId
      , Model.postText = fromColumn blogPostText
      , Model.postTitle = fromColumn blogPostTitle
      , Model.postAuthors = map (uncurry unpack) authors
      }
  pack Model.BlogPost {..} _ = (dpPost, dpAuthors)
    where
      dpPost =
        BlogPost
          { blogPostId = PrimaryKey $ Model.fromId postId
          , blogPostText = Column postText
          , blogPostTitle = Column postTitle
          }
      dpAuthors = (`pack` ((), ())) <$> postAuthors

instance HasDataProvider Model.LightPost where
  type DataProviderModel Model.LightPost = BlogPost
  type ParentRelations Model.LightPost = ()
  type ChildRelations Model.LightPost = ManyChildren Model.LightAuthor
  type MonadDataProvider Model.LightPost = Handler
  getPK _ = fromPK . blogPostId
  getID = Model.postId
  unpack BlogPost {..} authors =
    Model.BlogPost
      { Model.postId = Model.Id $ fromPK blogPostId
      , Model.postText = fromColumn blogPostText
      , Model.postTitle = fromColumn blogPostTitle
      , Model.postAuthors = Model.Unfilled
      }

instance HasDataProvider Model.LightAuthor where
  type DataProviderModel Model.LightAuthor = Author
  type ChildRelations Model.LightAuthor = EmptyChild
  type ParentRelations Model.LightAuthor = ()
  type MonadDataProvider Model.LightAuthor = Handler
  getPK _ = fromPK . authorId
  getID = Model.authorId
  pack Model.Author {Model.authorId = aId, Model.authorPseudonim = aP} _ =
    (dpAuthor, ())
    where
      dpAuthor =
        Author
          { authorId = PrimaryKey $ Model.fromId aId
          , authorPseudonim = Column aP
          }
  unpack Author {..} _ =
    Model.Author
      { Model.authorId = Model.Id $ fromPK authorId
      , Model.authorPseudonim = fromColumn authorPseudonim
      , authorPosts = Model.Unfilled
      }

instance DataProvider Handler where
  type DataProviderTypeClass Handler = MemoryStorable
  type CreateDataStructure Handler = Identity
  getAllEntities = pure . getList
  getEntityById proxy pk = pure $ find ((== pk) . getId) (getList proxy)
  createEntity (Identity model) = pure $ Just model

class MemoryStorable entity where
  getId :: entity -> Int
  getList :: Proxy entity -> [entity]

instance MemoryStorable Author where
  getId = fromPK . authorId
  getList _ = authors

instance MemoryStorable BlogPost where
  getId = fromPK . blogPostId
  getList _ = posts

instance MemoryStorable Auth where
  getId = fromPK . authId
  getList _ = auths

instance MemoryStorable User where
  getId = fromPK . userId
  getList _ = users
