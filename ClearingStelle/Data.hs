{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fglasgow-exts -XNoMonomorphismRestriction #-}


module ClearingStelle.Data
    (clearingStelleState
    ,module ClearingStelle.UserDB
    ,module ClearingStelle.Keys) where

import Data.Generics(Data)
import Data.Typeable
import Happstack.State

import ClearingStelle.UserDB
import ClearingStelle.Keys

------------------------------ AppState ------------------------------

data AppState = AppState
                deriving (Show, Read, Eq, Ord, Typeable, Data)
                         
instance Version AppState
$(deriveSerialize ''AppState)


instance Component AppState where
    type Dependencies AppState = UserDB :+: End
    initialValue = AppState

$(mkMethods ''AppState [])

clearingStelleState :: Proxy AppState
clearingStelleState = Proxy


