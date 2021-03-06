{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecordWildCards #-}

module Db.Model where

import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.Aeson (ToJSON, FromJSON)
import           Control.Monad.Reader
import           GHC.Generics (Generic)
import           Database.Persist.Sql
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      share, sqlSettings)

import           Config (Action, WithConfig, getPool)

share [mkMigrate "migrateAll", mkPersist sqlSettings] [persistLowerCase|

Course json
  program Text
  term Text
  code Text
  name Text
  level Text
  importance Text
  block Text
  credits Int
  wholeTerm Bool
  created UTCTime default=now()
  deriving Show
|]

  
doMigrations :: SqlPersistM ()
doMigrations = runMigration migrateAll

runDb :: SqlPersistT IO a -> WithConfig a
runDb query = do
  pool <- asks getPool
  liftIO $ runSqlPool query pool
