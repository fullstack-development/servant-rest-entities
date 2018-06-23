{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Examples.UsersWithBeamDB.Database where

import Data.Text (Text)
import Data.Time
import GHC.Generics (Generic)

import Database.Beam
import Database.Beam.Postgres

-- === MODELS ===
--
-- USER Model
data UserT f = User
  { _userId :: Columnar f Int
  , _userFirstName :: Columnar f Text
  , _userLastName :: Columnar f Text
  , _userCreatedAt :: Columnar f LocalTime
  , _userIsStaff :: Columnar f Bool
  , _userAuthId :: PrimaryKey AuthT f
  } deriving (Generic, Beamable)

type User = UserT Identity

type UserId = PrimaryKey UserT Identity

deriving instance Show User

deriving instance Eq User

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int)
                        deriving (Generic, Beamable)
  primaryKey = UserId . _userId

deriving instance Show (PrimaryKey UserT Identity)

deriving instance Eq (PrimaryKey UserT Identity)

users :: [UserT Identity]
users =
  [ User
      1
      "Ivan"
      "Sergeev"
      (LocalTime (ModifiedJulianDay 0) (TimeOfDay 0 0 0))
      True
      (AuthId 1)
  , User
      2
      "Sergey"
      "Ivanov"
      (LocalTime (ModifiedJulianDay 0) (TimeOfDay 0 0 0))
      False
      (AuthId 2)
  ]

--
-- AUTH Model
data AuthT f = Auth
  { _authId :: Columnar f Int
  , _authPassword :: Columnar f Text
  , _authCreatedAt :: Columnar f LocalTime
  } deriving (Generic, Beamable)

type Auth = AuthT Identity

type AuthId = PrimaryKey AuthT Identity

deriving instance Show Auth

deriving instance Eq Auth

instance Table AuthT where
  data PrimaryKey AuthT f = AuthId (Columnar f Int)
                        deriving (Generic, Beamable)
  primaryKey = AuthId . _authId

deriving instance Show (PrimaryKey AuthT Identity)

deriving instance Eq (PrimaryKey AuthT Identity)

data DemoBeamRestDb f = DemoBeamRestDb
  { _user :: f (TableEntity UserT)
  , _auth :: f (TableEntity AuthT)
  } deriving (Generic)

instance Database Postgres DemoBeamRestDb

demoBeamRestDb :: DatabaseSettings Postgres DemoBeamRestDb
demoBeamRestDb =
  defaultDbSettings `withDbModification`
  dbModification {_user = modifyTable (const "users") tableModification}
