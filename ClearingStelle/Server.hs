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
import Network.Socket
import Network.BSD

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
    roleAuth Admin $ msum [ dir "adduser" $ 
                                msum [ methodSP GET  $ admin_adduser_get
                                     , methodSP POST $ admin_adduser_post
                                     ]
                          , dir "deluser" $ uriRest admin_deluser
                          , dir "users" $ admin_users_get
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
          


clearingstelle = do
    let conf = nullConf
    s <- socket AF_INET Stream defaultProtocol
    setSocketOption s ReuseAddr 1
    h <- getHostByName "localhost"
    let p = toEnum $ port $ conf
    bindSocket s (SockAddrInet p (hostAddress h))
    listen s 10

    txCtrl <- startSystemState clearingStelleState 
    tid <- forkIO $ simpleHTTPWithSocket s conf server_part
    waitForTermination
    createCheckpoint txCtrl
    killThread tid
    shutdownSystem txCtrl
