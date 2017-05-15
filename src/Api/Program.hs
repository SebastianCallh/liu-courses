module Api.Program (Program, programs, isValidProgram) where

import            Data.Text (Text, unpack, toUpper)


data Program = D | IT | U | M | DPU | EM | Y |
               MED | KTS | MT | ED | I | Ii
             deriving (Show, Read, Eq)

programs :: [Program]
programs = [D, IT, U, M, DPU, EM, Y, MED, KTS, MT, ED, I, Ii]

isValidProgram :: Text -> Bool
isValidProgram program = (read . unpack $ toUpper program) `elem` programs
