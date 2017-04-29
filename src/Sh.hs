{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Sh (fetchCourses) where

import qualified Data.Text as T

import Text.XML.HXT.Core
import Text.HandsomeSoup
import Control.Monad
import Data.List.Split (chunksOf)
import Data.List
import Data.Text (Text, pack)
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Models

{-
data Level = G1 | G2 | A deriving (Show, Eq, Generic)

data Term = Ht17 | Ht27 | Vt18 | Vt28 | Ht19 | Ht29 deriving (Show, Eq, Generic)

data Importance = V | O | F | OV deriving (Show, Eq, Generic)

data Block = One | Two | Three | Four | None deriving (Show, Eq, Generic)

data Course = Course { term :: Term
                     , code :: Text
                     , name :: Text
                     , level :: Level
                     , importance :: Importance
                     , block :: Block
                     , credits :: Int
                     , wholeTerm :: Bool
                     } deriving (Show, Eq, Generic)
instance ToJSON Level
instance ToJSON Term
instance ToJSON Importance
instance ToJSON Block
--instance ToJSON Course

instance FromJSON Level
instance FromJSON Term
instance FromJSON Importance
instance FromJSON Block
--instance FromJSON Course
-}

coursesUrl = "http://kdb-5.liu.se/liu/lith/studiehandboken/action.lasso?&-response=lot_response.lasso&-op=eq&kp_budget_year=2017&-op=eq&kp_programkod=D&-op=eq&kp_programprofil=D&-op=gt&kp_termin=6"

fetchCourses :: IO [Course]
fetchCourses = do
  time <- getCurrentTime
  let doc = fromUrl coursesUrl
  let sameCode = \a b -> courseCode a == courseCode b
  let createCourses = nubBy sameCode . filterSuccessful . process time . filter (not . isTrash)
  let nTrashRows = 165

  markup <- runX $ doc >>> css "td" //> getText
  return . createCourses . drop nTrashRows $ markup

process :: UTCTime ->  [String] -> [Either String Course]
process time = doProcess ("", []) where
  doProcess :: (String, [Either String Course]) -> [String] -> [Either String Course]
  doProcess (term, cs) [] = cs
  doProcess (term, cs) xs
    | isTerm $ head xs = doProcess (head xs, cs) $ tail xs
    | otherwise = doProcess (term, c : cs) $ drop nFields xs where
        c = mkCourse time $ term : take nFields xs
        nFields = 6

filterSuccessful :: [Either String Course] -> [Course]
filterSuccessful = foldr succesFullyFetched [] where
  succesFullyFetched (Right c) cs = c : cs
  succesFullyFetched (Left _) cs = cs

mkCourse :: UTCTime -> [String] -> Either String Course
mkCourse time (t:c:n:l:i:b:cr:_) = Right Course { courseTerm = pack t
                                                 , courseCode = pack c
                                                 , courseName = pack n
                                                 , courseLevel = pack l
                                                 , courseImportance = pack i
                                                 , courseBlock = pack b
                                                 , courseCredits = 5--read $ filter (/='*') cr
                                                 , courseWholeTerm = elem '*' cr
                                                 , courseCreated = time
                                                 }

mkCourse _ args = Left $ "Wrong amount of args when making course: " ++ (show $ length args)

{-
mkTerm :: String -> Term
mkTerm s
  | s == "7Ht1" = Ht17
  | s == "7Ht2" = Ht27
  | s == "8Vt1" = Vt18
  | s == "8Vt2" = Vt28
  | s == "9Ht1" = Ht19
  | s == "9Ht2" = Ht19

mkLevel :: String -> Level
mkLevel s
  | s == "G1" = G1
  | s == "G2" = G2
  | s == "A" = A

mkBlock :: String -> Block
mkBlock s
  | s == "1" = One
  | s == "2" = Two
  | s == "3" = Three
  | s == "4" = Four
  | s == "-" = None

mkImportance :: String -> Importance
mkImportance s
  | s == "v" = V
  | s == "o" = O
  | s == "f" = F
  | s == "o/v" = OV

-}
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
isTrash xs = all isTrashChar xs where
  isTrashChar = flip elem ['\t',
                           '\r',
                           '\n',
                           '\160'
                          ]
