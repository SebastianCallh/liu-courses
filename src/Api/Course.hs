{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Api.Course (getCourses, getCourse,
                    insertCourse, insertCourses,
                    getCoursesByProgram, deleteCourses) where

import            Data.Text (Text, pack)
import            Control.Monad
import            Control.Monad.Trans (lift)
import            Web.Scotty.Trans (ScottyT, param, json, status, jsonData)

import            Database.Persist.Sql (toSqlKey)
import            Database.Persist.Postgresql (Entity (..), (==.), (>.), insertMany_,
                                     selectFirst, selectList, insert_, deleteWhere)

import            Network.HTTP.Types.Status (created201, internalServerError500,
                                             notFound404)

import            Config (runConfig, Action)
import            Db.Model
import            Db.Course 
import            Api.Core
import            Api.Program (Program, isValidProgram)

getCourses :: Action ()
getCourses = do
  courses <- (lift . selectCourses) []
  json courses
  
getCoursesByProgram :: Action ()
getCoursesByProgram = do
  program <- param "program"
  if isValidProgram $ program
    then let filter = [CourseProgram ==. program] in do
      courses <- (lift . selectCourses) filter
      json courses    
    else do fourOhFour

getCourse :: Action ()
getCourse = do
  code <- param "code"
  maybecourse <- (lift . selectCourse) [CourseCode ==. code]
  case maybecourse of
    Nothing -> fourOhFour
    Just course -> json course
