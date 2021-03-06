{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module RestEntities.Routing where

import GHC.TypeLits (Symbol)

-- import Handlers
import Servant.API

type RetrieveApi (name :: Symbol) view
   = name :> Capture "id" Int :> Get '[ JSON] view

type ListApi (name :: Symbol) view = name :> Get '[ JSON] [view]

type CreateApi (name :: Symbol) body view
   = name :> ReqBody '[ JSON] body :> Put '[ JSON] view

type UpdateApi (name :: Symbol) body view
   = name :> Capture "id" Int :> ReqBody '[ JSON] body :> Patch '[ JSON] view

type DeleteApi (name :: Symbol) = name :> Capture "id" Int :> Patch '[ JSON] ()

type CRUDApi (name :: Symbol) body view
   = RetrieveApi name view :<|> ListApi name view :<|> CreateApi name body view :<|> DeleteApi name :<|> UpdateApi name body view
