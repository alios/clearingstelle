{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fglasgow-exts #-}


module ClearingStelle.Server(clearingstelle) where

import System.Environment
import System.Exit
import System.Console.GetOpt
    
import Control.Monad
import Control.Concurrent
import Happstack.Server
import Happstack.Server.SimpleHTTP
import Happstack.State

import ClearingStelle.Data

server_part :: ServerPart Response
server_part = 
    msum [ dir "admin" $ admin_part
         ]

admin_part = 
    roleAuth Admin $
             msum  [ dir "adduser" $ methodSP POST $ admin_adduser_post
                   , dir "adduser" $ methodSP GET  $ admin_adduser_get
                   ]

clearingstelle =
    let conf = nullConf -- validateConf -- nullConf
    in do txCtrl <- startSystemState clearingStelleState 
          tid <- forkIO $ simpleHTTP conf server_part 
          waitForTermination
          killThread tid
          shutdownSystem txCtrl
