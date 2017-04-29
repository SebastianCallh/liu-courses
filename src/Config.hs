{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config(Config, Action, App, ConfigM,
              getEnv, getPool, getLogger, getHandler,
              getEnvironment, getOptions, getConfig,
              runConfigM) where

import           Data.Aeson (Value(..), object, (.=), encode)
import           Data.Text.Internal.Lazy (Text)
import qualified Data.Text.Lazy as T (pack)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS (pack, unpack)
import           Data.Default (def)

import           Text.Read (readEither)

import           Control.Monad.Trans.Maybe
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import           Control.Monad.Reader (MonadReader, ReaderT,
                                       asks, runReaderT)

import Network.Wai.Handler.Warp (Settings, defaultSettings,
                                 setFdCacheDuration, setPort, Port)

import           Database.Persist
import           Database.Persist.Postgresql (createPostgresqlPool,
                                              ConnectionPool, ConnectionString)
                 
import           System.Environment (lookupEnv)
import           Data.Monoid ((<>))

import Web.Scotty.Trans (ActionT, Options, ScottyT, status,
                         settings, verbose, json, showError)

import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import Network.HTTP.Types.Status (created201, internalServerError500,
                                   notFound404)

import Network.Wai (Middleware)

newtype ConfigM a = ConfigM { runConfigM :: ReaderT Config IO a }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO)

type App = ScottyT Text ConfigM
type Action = ActionT Text ConfigM
type Error = Text


data Environment
  = Development
  | Production
  | Test
  deriving (Read, Show)

data Config = Config
   { getEnv :: Environment
   , getPool :: ConnectionPool
   }

lookupVar :: String ->  IO (String)
lookupVar env = do
  maybeEnv <- lookupEnv env
  case maybeEnv of
    Nothing -> return . error $ "Env variable " ++ env ++ " does not exist!"
    Just envVar  -> return envVar

getEnvironment :: IO Environment
getEnvironment = read <$> lookupVar "ENV"

getPort :: IO Port
getPort = read <$> lookupVar "PORT"

getOptions :: Environment -> IO Options
getOptions env =  do
  set <- getSettings env
  return def
     { settings = set
     , verbose =  case env of
       Development -> 1
       Production -> 0
       Test -> 0
     }

getSettings :: Environment -> IO Settings
getSettings env = do
    port <- getPort
    let settings = (case env of
          Development -> setPort port . setFdCacheDuration 0
          _ -> setPort port)  defaultSettings
    return settings
  
getLogger :: Environment -> Middleware
getLogger Development = logStdoutDev
getLogger Production = logStdout
getLogger Test = id

getHandler :: Environment -> Error -> Action ()
getHandler env err = do
  status internalServerError500
  let o = case env of
        Development -> object ["error" .= showError err]
        Production -> Null
        Test -> object ["error" .= showError err]
  json o

makePool :: Environment -> IO ConnectionPool
makePool Test = runNoLoggingT $ createPostgresqlPool (conStr "_test") (poolSize Test)
makePool Development = runStdoutLoggingT $ createPostgresqlPool (conStr "")  (poolSize Development)
makePool Production = do
  pool <- runMaybeT $ do
    let keys = [ "host="
               , "port="
               , "user="
               , "password="
               , "dbname="
               ]
        envs = [ "PGHOST"
               , "PGPORT"
               , "PGUSER"
               , "PGPASS"
               , "PGDATABASE"
               ]
    envVars <- traverse (MaybeT . lookupEnv) envs
    let conStr = mconcat . zipWith (++) keys $ envVars
    runStdoutLoggingT $ createPostgresqlPool (BS.pack . read $ conStr) $ poolSize Production
  case pool of
    Nothing -> error "Environmental variables missing"
    Just a -> return a

poolSize :: Environment -> Int
poolSize env = case env of
                   Development -> 1
                   Production  -> 8
                   Test        -> 1  

conStr :: ByteString -> ConnectionString
conStr suffix = "host=localhost dbname=liu-courses" <> suffix <>
                " user=postgres password=psql port=5432"

getConfig :: IO Config
getConfig = do
  env <- getEnvironment
  pool <- makePool env
  return $ Config { getEnv = env
                  , getPool = pool
                  }
