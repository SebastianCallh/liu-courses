{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Sh (fetchCourses) where

import qualified Data.Text as T

import           Text.XML.HXT.Core
import           Text.HandsomeSoup
import           Control.Monad
import           Data.List.Split (chunksOf)
import           Data.List
import           Data.Text (Text, pack)
import           Text.Printf
import           GHC.Generics
import           Data.Aeson (ToJSON, FromJSON)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Control.Monad
import           Data.Char

import           Debug.Trace as D

import           Models

data Program = D | IT | U | M | DPU | EM | Y |
               MED | KTS | MT | ED | I | Ii
             deriving (Show, Read)

urlForProgram :: Program -> Int -> String
urlForProgram prog year = printf url (show year) (show prog) (show prog) where
  url = "http://kdb-5.liu.se/liu/lith/studiehandboken/action.lasso?&-response=lot_response.lasso&-op=eq&kp_budget_year=%s&-op=eq&kp_programkod=%s&-op=eq&kp_programprofil=%s&-op=gt&kp_termin=6" 


programs = [D, IT, U, M, DPU, EM, Y, MED, KTS, MT, I, Ii]
year = 2017

fetchCourses :: IO [Course]
fetchCourses = fmap concat $ forM programs $ fetchProgramCourses year

fetchProgramCourses :: Int -> Program -> IO [Course]
fetchProgramCourses year program = do
  time <- getCurrentTime
  let doc = fromUrl $ urlForProgram program year
  let sameCode = \a b -> courseCode a == courseCode b
  let createCourses = nubBy sameCode .
                      process time program .
                      filter (not . isTrash)
                      
  markup <- runX $ doc >>> css "td" //> getText
  -- For some reason you get everything duplicated from runX
  let courseMarkup =  dropWhile (/= "7Ht1") . tail . dropWhile (/= "7Ht1") $ markup
  let courses = createCourses . filter (not .isTrash ) $ courseMarkup
  print $ "Fetched " ++ (show $ length courses) ++ " courses for program " ++ show program
  return courses

process :: UTCTime ->  Program -> [String] -> [Course]
process time program = doProcess ("", []) where
  doProcess :: (String, [Course]) -> [String] -> [Course]
  doProcess (term, cs) [] = cs

  
  doProcess (term, cs) ("TFBI17":n:l:i:cr:rest) = doProcess (term, c : cs) $ rest where
    c = mkCourse time program term ("TFBI17":n:l:i:"-":cr:[])
    
  -- TFBI17 has an empty td in the "block" column
  doProcess (term, cs) xs
    | isTerm $ head xs = doProcess (head xs, cs) $ tail xs
    | otherwise = doProcess (term, c : cs) $ drop nFields xs where
        c = mkCourse time program term $ take nFields xs
        nFields = 6

mkCourse :: UTCTime -> Program -> String -> [String] -> Course--Either String Course
mkCourse time program term (c:n:l:i:b:cr:[]) = do
  Course { courseProgram = pack . show $ program
         , courseTerm = pack term
         , courseCode = pack c
         , courseName = pack n
         , courseLevel = pack l
         , courseImportance = pack i
         , courseBlock = pack b
         , courseCredits = read $ filter isNumber cr
         , courseWholeTerm = elem '*' cr
         , courseCreated = time
         }

mkCourse _ _ term args =
  error $ "Wrong amount of args when making course: " ++ term ++ " " ++ (show args)

isTerm :: String -> Bool
isTerm = flip elem terms where
  terms = ["7Ht1",
           "7Ht2",
           "8Vt1",
           "8Vt2",
           "9Ht1",
           "9Ht2"
          ]

isTrash :: String -> Bool
isTrash = all isTrashChar where
  isTrashChar = flip elem ['\t',
                           '\r',
                           '\n',
                           '\160'
                          ]
