module Cron where

import Database.Persist.Postgresql (runSqlPool, insertMany_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans

import Config (getConfig, getPool)
import Api.Course (insertCourse)
import Sh (fetchCourses)

main :: IO ()
main = do
  config <- getConfig
  courses <- liftIO fetchCourses
  let query  = insertMany_ courses
  runSqlPool query $ getPool config
  print $ "Saved " ++ (show $ length courses) ++ " courses"
  
  
  
