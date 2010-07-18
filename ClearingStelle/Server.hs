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
    msum  [ dir "adduser" $ methodSP POST $ roleAuth adminRole $ admin_adduser
          ]

admin_adduser = 
    undefined

clearingstelle =
    let conf = nullConf
    in do txCtrl <- startSystemState clearingStelleState 
          tid <- forkIO $ simpleHTTP conf server_part 
          waitForTermination
          killThread tid
          shutdownSystem txCtrl
