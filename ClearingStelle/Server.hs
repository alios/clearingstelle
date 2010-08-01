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
         , dir "manager" $ manager_part
         , dir "invitesite" $ inviteSite_part
         , dir "refsite" $ refSite_part
         , methodSP GET $ fetchkey_get
         , methodSP POST $ fetchkey_post
         ]

admin_part = 
    roleAuth Admin $ dir "adduser" $ 
             msum [ methodSP GET  $ admin_adduser_get
                  , methodSP POST $ admin_adduser_post
                  ]
                  

manager_part = 
    roleAuth Manager $ dir "createkeyset" $ 
             msum [ methodSP GET $ manager_createkeyset_get
                  , methodSP POST $ manager_createkeyset_post
                  ]


inviteSite_part =
    roleAuth InviteSite $ dir "getkeys" $ uriRest $ inviteSite_getkeys

refSite_part =
    roleAuth RefSite $ dir "getkeys" $ uriRest $ refSite_getkeys
          


clearingstelle =
    let conf = nullConf -- validateConf -- nullConf
    in do txCtrl <- startSystemState clearingStelleState 
          tid <- forkIO $ simpleHTTP conf server_part 
          waitForTermination
          createCheckpoint txCtrl
          killThread tid
          shutdownSystem txCtrl
