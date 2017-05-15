module Cron where

import           Control.Monad.IO.Class (liftIO)
import           Config (WithConfig, App, runConfig, getConfig, getPool)
import           Control.Monad.Reader (runReaderT)
  
import           Config (runConfig)
import           Sh (fetchCourses)
import           Db.Course (deleteCourses, insertCourses)

main :: IO ()
main = do
  config <- getConfig
  runReaderT (runConfig updateCourses) config
  
updateCourses :: WithConfig ()
updateCourses = do
  config <- liftIO getConfig
  courses <- liftIO fetchCourses
  deleteCourses
  liftIO $ print "Deleted old courses"
  insertCourses courses
  liftIO $ print $ "Saved " ++ (show $ length courses) ++ " courses"  
  
