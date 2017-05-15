module Db.Course (selectCourse, selectCourses,
                  insertCourse, insertCourses,
                  deleteCourses) where

import            Database.Persist.Postgresql (Entity (..), Filter, (==.), (>.), insertMany_,
                                               selectFirst, selectList, insert, deleteWhere,
                                               toSqlKey)

import            Config (WithConfig, Action)
import            Db.Model 


selectCourse :: [Filter Course] -> WithConfig (Maybe (Entity Course))
selectCourse filter = do
  course <- runDb $ selectFirst filter []
  return course

selectCourses :: [Filter Course] -> WithConfig [Entity Course]
selectCourses filter = do
  courses <- runDb $ selectList filter []
  return courses

insertCourse :: Course -> WithConfig (Key Course)
insertCourse course = do
  key <- runDb $ insert course
  return key

insertCourses :: [Course] -> WithConfig ()
insertCourses courses = do
  runDb $ insertMany_ courses
  
deleteCourses :: WithConfig ()
deleteCourses = do
  runDb $ deleteWhere [CourseId >. toSqlKey 0] 

