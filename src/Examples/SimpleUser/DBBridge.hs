{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Examples.SimpleUser.DBBridge where

import DBEntity
import qualified Examples.SimpleUser.DB as DB
import Examples.SimpleUser.Model
import Model

instance DBConvertable Auth DB.Auth where
  dbConvertTo Auth {..} (Just user) = (dbAuth, ())
    where
      dbAuth =
        DB.Auth
        { DB.authId =
            if isIdEmpty authId
              then DB.def
              else DB.PrimaryKey (fromId authId)
        , DB.authPassword = DB.Column authPassword
        , DB.authCreatedAt = DB.Column authCreatedAt
        , DB.authUserId = DB.ForeignKey (DB.PrimaryKey (fromId $ userId user))
        }
  dbConvertFrom DB.Auth {..} _ =
    Auth
    { authId = Id $ DB.fromPK authId
    , authPassword = DB.fromColumn authPassword
    , authCreatedAt = DB.fromColumn authCreatedAt
    }

instance DBConvertable User DB.User where
  dbConvertTo user@User {..} _ = (dbUser, dbAuth)
    where
      (dbAuth, rels) = dbConvertTo userAuth (Just user)
      dbUser =
        DB.User
        { userId = DB.PrimaryKey (fromId userId)
        , userFirstName = DB.Column userFirstName
        , userLastName = DB.Column userLastName
        , userCreatedAt = DB.Column userCreatedAt
        , userIsStaff = DB.Column userIsStaff
        }
  dbConvertFrom DB.User {..} (Just auth) =
    User
    { userId = Id $ DB.fromPK userId
    , userFirstName = DB.fromColumn userFirstName
    , userLastName = DB.fromColumn userLastName
    , userIsStaff = DB.fromColumn userIsStaff
    , userCreatedAt = DB.fromColumn userCreatedAt
    , userAuth = dbConvertFrom auth Nothing
    }
  dbConvertFrom _ Nothing =
    error "You should pass all relations to user db converter."
