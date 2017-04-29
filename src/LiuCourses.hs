{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LiuCourses (runApp, runMigrations) where

import           Web.Scotty.Trans (ScottyT, ActionT, param, status, middleware,
                                   scottyOptsT, scottyT, json, get, html,
                                   defaultHandler, notFound, post)

import           Data.Text (Text)
import           Text.Read (readEither)

import           Control.Monad.IO.Class (MonadIO, liftIO)


import           Data.Aeson (Value (Null), (.=), object)

import           Control.Monad.Reader (ReaderT, runReaderT)

import           Database.Persist.Postgresql (Entity (..), (==.),
                                     selectFirst, selectList,
                                     runSqlPersistMPool, runMigration)

import           Config 
import           Models
import           Api.Course
import           Api.Core


app :: Config ->  App ()
app config = do
  let env = getEnv config
  middleware (getLogger env)
  defaultHandler (getHandler env)
  
  get       "/courses"       getCourses
  post      "/courses"       insertCourse
  get       "/course/:code"  getCourse
  notFound                   fourOhFour

runApp :: Config -> IO ()
runApp config = do
  environment <- getEnvironment
  options <- getOptions environment
  let reader m = runReaderT (runConfigM m) config
  scottyOptsT options reader $ app config

runMigrations :: Config -> IO ()
runMigrations config =
  liftIO $ flip runSqlPersistMPool (getPool config) $ runMigration migrateAll
