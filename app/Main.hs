module Main where

import           LiuCourses (runApp, runMigrations)
import           Config (getConfig)

main :: IO ()
main = do
  config <- getConfig
  runMigrations config
  runApp config
