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

type instance DataProviderModel Model.Auth = Auth

type instance DataProviderModel Model.User = User

instance HasDataProvider Model.User User where
  type MonadDataProvider Model.User = Handler
  type ChildRelations Model.User = Model.Auth
  type ParentRelations Model.User = ()
  save = pure
  deleteById _ _ = pure $ Right ()
  loadById _ pk =
    runMaybeT $ do
      user <- wrap $ find (\u -> fromPK (userId u) == pk) users
      auth <- wrap $ find (\a -> fromFK (authUserId a) == userId user) auths
      return $ unpack user (Just auth)
    where
      wrap = MaybeT . return
  loadAll _ = pure $ map (\u -> unpack u (authFor u)) users
    where
      authFor user = find (authByUserId $ userId user) auths
      authByUserId id auth = fromFK (authUserId auth) == id
  unpack User {..} (Just auth) =
    Model.User
    { userId = Model.Id $ fromPK userId
    , userFirstName = fromColumn userFirstName
    , userLastName = fromColumn userLastName
    , userIsStaff = fromColumn userIsStaff
    , userCreatedAt = fromColumn userCreatedAt
    , userAuth = unpack auth Nothing
    }
  unpack _ Nothing = error "You should pass all relations to user db converter."
  pack user@Model.User {..} _ = (providedUser, providedAuth)
    where
      (providedAuth, _) = pack userAuth (Just user)
      providedUser =
        User
        { userId = PrimaryKey (Model.fromId userId)
        , userFirstName = Column userFirstName
        , userLastName = Column userLastName
        , userCreatedAt = Column userCreatedAt
        , userIsStaff = Column userIsStaff
        }

instance HasDataProvider Model.Auth Auth where
  type ParentRelations Model.Auth = Model.User
  type ChildRelations Model.Auth = ()
  type MonadDataProvider Model.Auth = Handler
  save = pure
  deleteById _ _ = pure $ Right ()
  loadById _ pk =
    pure $ flip unpack Nothing <$> find (\a -> fromPK (authId a) == pk) auths
  loadAll _ = pure $ flip unpack Nothing <$> auths
  pack Model.Auth {..} (Just user) = (dbAuth, ())
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
