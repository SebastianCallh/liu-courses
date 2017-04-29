{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Api.Course (getCourses, getCourse, insertCourse) where

import            Data.Text (Text)
import            Control.Monad
import            Web.Scotty.Trans (ScottyT, param, json, status, jsonData)

import            Database.Persist.Postgresql (Entity (..), (==.),
                                     selectFirst, selectList, insert_)

import            Network.HTTP.Types.Status (created201, internalServerError500,
                                             notFound404)

import            Models
import            Api.Core
import            Config (Action)

getCourses :: Action ()
getCourses = do
  list <- runDb $ selectList [] []
  json (list :: [Entity Course])

getCourse :: Action ()
getCourse = do
  code <- param "code"
  maybecourse <- runDb $ selectFirst [CourseCode ==. code] []
  case maybecourse of
    Nothing -> fourOhFour
    Just course -> json course

insertCourse :: Action ()
insertCourse = do
  course <- jsonData
  runDb $ insert_ course
  status created201
  json (course :: Course)
