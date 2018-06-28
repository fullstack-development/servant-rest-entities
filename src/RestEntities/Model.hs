{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module RestEntities.Model where

import Data.Aeson
import GHC.Generics

data Id a
  = Empty
  | Id a
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data NotRich =
  Unfilled

fromId (Id v) = v
fromId Empty = error "Could not unpack empty id of model"

isIdEmpty Empty = True
isIdEmpty _ = False
