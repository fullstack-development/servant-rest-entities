{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Examples.UsersWithBeamDB.GenericBeam where

import Control.Monad
import Data.Proxy
import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import GHC.Generics

import DataProvider
import Examples.UsersWithBeamDB.ServerConfig

import qualified Examples.UsersWithBeamDB.Database as DB

type TableSelector table
   = (DatabaseSettings Postgres DB.DemoBeamRestDb -> DatabaseEntity Postgres DB.DemoBeamRestDb (TableEntity table))

selectQueryAll ::
     (Table table)
  => TableSelector table
  -> Q PgSelectSyntax DB.DemoBeamRestDb s (table (QExpr PgExpressionSyntax s))
selectQueryAll tableSelector = all_ (tableSelector DB.demoBeamRestDb)

selectAll ::
     (Table table)
  => TableSelector table
  -> SqlSelect PgSelectSyntax (table Identity)
selectAll = select . selectQueryAll

queryGetAll ::
     ( Table table
     , Generic (table Identity)
     , Generic (table Exposed)
     , FromBackendRow Postgres (table Identity)
     )
  => TableSelector table
  -> Pg [table Identity]
queryGetAll = runSelectReturningList . selectAll

class ( Generic e
      , DB.IdentityToTable e Identity ~ e
      , Generic (DB.IdentityToTable e Identity)
      , Generic (DB.IdentityToTable e Exposed)
      , FromBackendRow Postgres (DB.IdentityToTable e Identity)
      , Table (DB.IdentityToTable e)
      ) =>
      BeamStorable (e :: *)
  where
  beamTableSelector :: Proxy e -> TableSelector (DB.IdentityToTable e)

instance BeamStorable DB.User where
  beamTableSelector _ = DB._user

instance DataProvider ServerConfigReader where
  type DataProviderTypeClass ServerConfigReader = BeamStorable
  getAllEntities ::
       (BeamStorable dbmodel) => Proxy dbmodel -> ServerConfigReader [dbmodel]
  getAllEntities proxyEntity =
    runDS $ queryGetAll $ beamTableSelector proxyEntity
    -- results <- runDS $ queryGetAll $ beamTableSelector proxyEntity
    -- forM results $ \entity -> do
    --   rels <-
    --     runAction $
    --     applyReduced
    --       (runDS . getAllEntities)
    --       (undefined :: MapProxyToRels (Proxy (MapDataProviders (ChildRelations (ModelOfDataProvider dbmodel))))) -- :: ServerConfigReader (MapDataProviders (ChildRelations (ModelOfDataProvider dbmodel)))
    --   pure (entity, rels)
    -- pure $ map (, undefined) result
