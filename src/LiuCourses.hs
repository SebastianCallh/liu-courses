{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LiuCourses (runApp, runMigrations) where

import           Web.Scotty.Trans (middleware, scottyOptsT,
                                   json, get, defaultHandler, notFound)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (runReaderT)
import           Database.Persist.Postgresql (runSqlPersistMPool, runMigration)

import           Config 
import           Db.Model
import           Api.Course
import           Api.Core

app :: Config ->  App ()
app config = do
  let env = getEnv config
  middleware (getLogger env)
  defaultHandler (getHandler env)
  
  get       "/courses"          getCourses
  get       "/courses/:program" getCoursesByProgram
  get       "/course/:code"     getCourse
  notFound                      fourOhFour

runApp :: Config -> IO ()
runApp config = do
  environment <- getEnvironment
  options <- getOptions environment
  let reader m = runReaderT (runConfig m) config
  scottyOptsT options reader $ app config

runMigrations :: Config -> IO ()
runMigrations config =
  liftIO $ flip runSqlPersistMPool (getPool config) $ runMigration migrateAll
