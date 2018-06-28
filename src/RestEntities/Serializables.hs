{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module RestEntities.Serializables where

class Serializable e to | to -> e where
  serialize :: e -> to

class Deserializable e from | from -> e where
  deserialize :: Maybe Int -> from -> IO e
