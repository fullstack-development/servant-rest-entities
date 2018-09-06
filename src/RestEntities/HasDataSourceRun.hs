{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RestEntities.HasDataSourceRun where

class HasDataSourceRun (actionMonad :: * -> *) (dsMonad :: * -> *) where
  runDS :: dsMonad a -> actionMonad a
