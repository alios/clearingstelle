{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fglasgow-exts #-}


module ClearingStelle.Server where

import Control.Monad
import Happstack.Server


server_part :: ServerPartT IO Response
server_part = 
    msum [ dir "admin" admin_part
         ]

admin_part = 
    msum  []

clearingstelle =
    let conf = nullConf
    in simpleHTTP conf server_part 
