module Api.Core (fourOhFour) where

import Config (Action)
import Web.Scotty.Trans (status, json)
import Data.Aeson (Value (Null))
import Network.HTTP.Types.Status (notFound404)

fourOhFour :: Action ()
fourOhFour = do
   status notFound404
   json Null
