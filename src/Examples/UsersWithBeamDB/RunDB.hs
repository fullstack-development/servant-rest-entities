{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Examples.UsersWithBeamDB.RunDB where

import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time
import GHC.Generics (Generic)

import Database.Beam.Postgres

import Examples.UsersWithBeamDB.Database
import Examples.UsersWithBeamDB.ServerConfig

createPgConn =
  connectPostgreSQL "postgresql://demo@localhost:5432/demorestwithbeam"

runDBAction :: Pg a -> Connection -> IO a
runDBAction query conn = runBeamPostgresDebug putStrLn conn query

runDBWithSeparateConn query = createPgConn >>= runDBAction query

initDB :: ((forall q. Pg q -> IO q) -> IO a) -> IO a
initDB f =
  bracket createPgConn close $ \conn -> f (runBeamPostgresDebug putStrLn conn)
