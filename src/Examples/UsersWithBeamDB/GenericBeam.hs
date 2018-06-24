{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Examples.UsersWithBeamDB.GenericBeam where

import Data.Maybe (listToMaybe)
import Data.Proxy
import Database.Beam
import Database.Beam.Backend.SQL
import qualified Database.Beam.Backend.SQL.BeamExtensions as BeamExtensions
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.Beam.Schema.Tables

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

selectQueryFilteredByPk ::
     ( Table table
     , FieldsFulfillConstraint (HasSqlEqualityCheck PgExpressionSyntax) (PrimaryKey table)
     )
  => TableSelector table
  -> PrimaryKey table (QExpr PgExpressionSyntax s)
  -> Q PgSelectSyntax DB.DemoBeamRestDb s (table (QExpr PgExpressionSyntax s))
selectQueryFilteredByPk tableSelector idvalue =
  filter_ (\entity -> pk entity ==. idvalue) $
  all_ (tableSelector DB.demoBeamRestDb)

selectByPK ::
     ( Table table
     , Num b
     , FieldsFulfillConstraint (HasSqlEqualityCheck PgExpressionSyntax) (PrimaryKey table)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) (PrimaryKey table)
     )
  => TableSelector table
  -> (b -> PrimaryKey table Identity)
  -> Int
  -> SqlSelect PgSelectSyntax (table Identity)
selectByPK tableSelector pkConstructor =
  select .
  selectQueryFilteredByPk tableSelector . val_ . pkConstructor . fromIntegral

queryGetByPK ::
     ( Table table
     , Num b
     , FieldsFulfillConstraint (HasSqlEqualityCheck PgExpressionSyntax) (PrimaryKey table)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) (PrimaryKey table)
     , FromBackendRow Postgres (table Identity)
     )
  => TableSelector table
  -> (b -> PrimaryKey table Identity)
  -> Int
  -> Pg (Maybe (table Identity))
queryGetByPK tableSelector pkConstructor =
  runSelectReturningOne . selectByPK tableSelector pkConstructor

runQueryFromValues ::
     ( Table table
     , FromBackendRow Postgres (table Identity)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) table
     )
  => TableSelector table
  -> table Identity
  -> Pg [table Identity]
runQueryFromValues table entity =
  BeamExtensions.runInsertReturningList (table DB.demoBeamRestDb) $
  insertExpressions [val_ entity]

createFromValues ::
     ( Table table
     , FromBackendRow Postgres (table Identity)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) table
     )
  => TableSelector table
  -> table Identity
  -> Pg (Maybe (table Identity))
createFromValues table entity = listToMaybe <$> runQueryFromValues table entity

runQueryFromExpr ::
     ( Table table
     , FromBackendRow Postgres (table Identity)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) table
     )
  => TableSelector table
  -> (forall s. table (QExpr PgExpressionSyntax s))
  -> Pg [table Identity]
runQueryFromExpr table entity =
  BeamExtensions.runInsertReturningList (table DB.demoBeamRestDb) $
  insertExpressions [entity]

createFromExpr ::
     ( Table table
     , FromBackendRow Postgres (table Identity)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) table
     )
  => TableSelector table
  -> (forall s. table (QExpr PgExpressionSyntax s))
  -> Pg (Maybe (table Identity))
createFromExpr table entity = listToMaybe <$> runQueryFromExpr table entity

class ( Generic e
      , DB.IdentityToTable e Identity ~ e
      , Generic (DB.IdentityToTable e Identity)
      , Generic (DB.IdentityToTable e Exposed)
      , FromBackendRow Postgres (DB.IdentityToTable e Identity)
      , Table (DB.IdentityToTable e)
      , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) (DB.IdentityToTable e)
      , FieldsFulfillConstraint (HasSqlEqualityCheck PgExpressionSyntax) (PrimaryKey (DB.IdentityToTable e))
      , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) (PrimaryKey (DB.IdentityToTable e))
      ) =>
      BeamStorable (e :: *)
  where
  beamTableSelector :: Proxy e -> TableSelector (DB.IdentityToTable e)
  beamTablePKConstructor ::
       Proxy e -> (Int -> PrimaryKey (DB.IdentityToTable e) Identity)

newtype BeamCreateStructure e =
  BeamCreateStructure (forall s. DB.IdentityToTable e (QExpr PgExpressionSyntax s))

instance BeamStorable DB.User where
  beamTableSelector _ = DB._user
  beamTablePKConstructor _ = DB.UserId

instance BeamStorable DB.Auth where
  beamTableSelector _ = DB._auth
  beamTablePKConstructor _ = DB.AuthId

instance DataProvider ServerConfigReader where
  type DataProviderTypeClass ServerConfigReader = BeamStorable
  type CreateDataStructure ServerConfigReader = BeamCreateStructure
  getAllEntities ::
       (BeamStorable dbmodel) => Proxy dbmodel -> ServerConfigReader [dbmodel]
  getAllEntities proxyEntity =
    runDS $ queryGetAll $ beamTableSelector proxyEntity
  getEntityById ::
       (BeamStorable dbmodel)
    => Proxy dbmodel
    -> Int
    -> ServerConfigReader (Maybe dbmodel)
  getEntityById proxyEntity entityId =
    runDS $
    queryGetByPK
      (beamTableSelector proxyEntity)
      (beamTablePKConstructor proxyEntity)
      entityId
  createEntity ::
       (BeamStorable dbmodel)
    => Proxy dbmodel
    -> BeamCreateStructure dbmodel
    -> ServerConfigReader (Maybe dbmodel)
  createEntity proxyEntity (BeamCreateStructure dataStructure) =
    runDS $ createFromExpr (beamTableSelector proxyEntity) dataStructure

innerJoin ::
     ( FieldsFulfillConstraint (HasSqlEqualityCheck PgExpressionSyntax) (PrimaryKey table)
     , Generic (PrimaryKey table Exposed)
     , Generic (PrimaryKey table Identity)
     , Table table
     )
  => t
  -> (t -> PrimaryKey table (QExpr PgExpressionSyntax s))
  -> TableSelector table
  -> Q PgSelectSyntax DB.DemoBeamRestDb s (table (QExpr PgExpressionSyntax s))
innerJoin baseEntity field relEntity = do
  enitity <- all_ (relEntity DB.demoBeamRestDb)
  guard_ (field baseEntity `references_` enitity)
  return enitity

class ( DB.IdentityToTable dbmodelA Identity ~ dbmodelA
      , DB.IdentityToTable dbmodelB Identity ~ dbmodelB
      ) =>
      HasRelation dbmodelA dbmodelB
  where
  baseEntity :: Proxy dbmodelB -> TableSelector (DB.IdentityToTable dbmodelA)
  relEntity :: Proxy dbmodelA -> TableSelector (DB.IdentityToTable dbmodelB)
  joinField ::
       (DB.IdentityToTable dbmodelA (QExpr PgExpressionSyntax s) -> PrimaryKey (DB.IdentityToTable dbmodelB) (QExpr PgExpressionSyntax s))
  innerJoinRelation ::
       ( FieldsFulfillConstraint (HasSqlEqualityCheck PgExpressionSyntax) (PrimaryKey (DB.IdentityToTable dbmodelA))
       , Generic (PrimaryKey (DB.IdentityToTable dbmodelA) Exposed)
       , Generic (PrimaryKey (DB.IdentityToTable dbmodelA) Identity)
       , Table (DB.IdentityToTable dbmodelA)
       , FieldsFulfillConstraint (HasSqlEqualityCheck PgExpressionSyntax) (PrimaryKey (DB.IdentityToTable dbmodelB))
       , Generic (PrimaryKey (DB.IdentityToTable dbmodelB) Exposed)
       , Generic (PrimaryKey (DB.IdentityToTable dbmodelB) Identity)
       , Table (DB.IdentityToTable dbmodelB)
       )
    => DB.IdentityToTable dbmodelA (QExpr PgExpressionSyntax s)
    -> Q PgSelectSyntax DB.DemoBeamRestDb s ((DB.IdentityToTable dbmodelB) (QExpr PgExpressionSyntax s))
  innerJoinRelation baseEntity = do
    enitity <- all_ (relEntity (Proxy :: Proxy dbmodelA) DB.demoBeamRestDb)
    guard_ (joinField baseEntity `references_` enitity)
    return enitity

instance HasRelation DB.User DB.Auth where
  baseEntity _ = DB._user
  relEntity _ = DB._auth
  joinField = DB._userAuthId
