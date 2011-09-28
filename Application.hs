{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( withCS
    , withDevelAppPort
    ) where

import Foundation
import Settings
import Yesod.Static
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Logger (Logger)
import Data.ByteString (ByteString)
import Data.Dynamic (Dynamic, toDyn)
import qualified Database.Persist.Base
import Database.Persist.GenericSql (runMigration)
import Control.Concurrent

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Clearingstelle

dom0Title = "dom0"

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "CS" resourcesCS

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "config/favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: ByteString)

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withCS :: AppConfig DefaultEnv -> Logger -> (Application -> IO ()) -> IO ()
withCS conf logger f = do
#ifdef PRODUCTION
    s <- static Settings.staticDir
#else
    s <- staticDevel Settings.staticDir
#endif
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
            $ either error return . Database.Persist.Base.loadConfig
    Database.Persist.Base.withPool (dbconf :: Settings.PersistConfig) $ \p -> do
        Database.Persist.Base.runPool dbconf migration p        
        kfTID <- forkIO (keyFactory dbconf p)
        let h = CS conf logger s p
        defaultRunner f h
          where migration = do
                  runMigration migrateAll
                  dom0' <- selectDomain dom0Title
                  case (dom0') of
                    Just _ -> return ()
                    Nothing -> do
                      dom0id <- insert $ Domain dom0Title
                      uid <- insert $ User "admin" (Just "admin")
                      insert $ Role uid AdminRole dom0id
                      return ()
                  
-- for yesod devel
withDevelAppPort :: Dynamic
withDevelAppPort = toDyn $ defaultDevelApp withCS

keyFactory dbconf p = do
  cnt <- Database.Persist.Base.runPool dbconf (completeKeysets 1000) p
  if (cnt > 0)
    then do print $ "created " ++ show cnt ++ " new keys"
    else do threadDelay 1000000
  keyFactory dbconf p