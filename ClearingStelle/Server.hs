{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fglasgow-exts #-}


module ClearingStelle.Server where

import Control.Monad
import Happstack.Server


server_part :: ServerPartT IO Response
server_part = 
    msum [ dir "admin" admin_part
         , dir "auth" $ methodSP POST $ admin_auth
         ]

admin_part = 
    msum  [ dir "adduser" $  methodSP POST $ admin_adduser
          ]




admin_adduser = 
    undefined

admin_auth =
    undefined



clearingstelle =
    let conf = nullConf
    in simpleHTTP conf server_part 

main = clearingstelle