{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module RestEntities.Examples.UsersWithBeamDB.RunDB where

import Control.Exception (bracket)

import Database.Beam.Postgres

createPgConn =
  connectPostgreSQL "postgresql://demo@localhost:5432/demorestwithbeam"

runDBAction :: Pg a -> Connection -> IO a
runDBAction query conn = runBeamPostgresDebug putStrLn conn query

runDBWithSeparateConn query = createPgConn >>= runDBAction query

initDB :: ((forall q. Pg q -> IO q) -> IO a) -> IO a
initDB f =
  bracket createPgConn close $ \conn -> f (runBeamPostgresDebug putStrLn conn)
