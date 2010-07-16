{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fglasgow-exts #-}


module ClearingStelle.Server where

import System.Environment
import System.Exit
import System.Console.GetOpt
    
import Control.Monad
import Happstack.Server
import Happstack.Server.SimpleHTTP
import ClearingStelle.Data
import Happstack.State

server_part :: ServerPartT IO Response
server_part = 
    msum [ dir "admin" admin_part
         , dir "auth" $ methodSP POST $ auth
         ]

admin_part = 
    msum  [ dir "adduser" $  methodSP POST $ admin_adduser
          ]



admin_adduser = 
    undefined

auth = 
    do usermap <- undefined-- query getUserMap
       basicAuth "please authorize yourself" usermap $ return $ toResponse "authorized"

clearingstelle =
    let conf = nullConf
    in do txCtrl <- startSystemState clearingStelleState 
          simpleHTTP conf server_part 
          waitForTermination

