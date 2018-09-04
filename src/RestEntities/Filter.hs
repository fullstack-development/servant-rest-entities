{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module RestEntities.Filter where

import Data.Proxy
import Data.Typeable
import GHC.Generics
import GHC.TypeLits

data Filter entity (field :: Symbol)
  = ByEqField (Proxy field)
              (FilterFieldValue entity field)
  | ByContainingFieldIn (Proxy field)
                        [FilterFieldValue entity field]
  deriving (Generic, Typeable)

type family FilterFieldValue entity (field :: Symbol)
