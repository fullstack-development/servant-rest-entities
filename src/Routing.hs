{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Routing where

import GHC.Generics
import GHC.TypeLits (Symbol)

-- import Handlers
import Model
import Servant.API

type RetrieveApi (name :: Symbol) view
   = name :> Capture "id" Integer :> Get '[ JSON] view

type ListApi (name :: Symbol) view = name :> Get '[ JSON] [view]

type CreateApi (name :: Symbol) body view
   = name :> ReqBody '[ JSON] body :> Put '[ JSON] view

type UpdateApi (name :: Symbol) body view
   = name :> Capture "id" Integer :> Patch '[ JSON] view

type CRUDApi (name :: Symbol) body view
   = RetrieveApi name view :<|> ListApi name view :<|> CreateApi name body view
