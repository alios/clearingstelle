{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fglasgow-exts -XDeriveDataTypeable -XStandaloneDeriving #-}


module ClearingStelle.Data where

import Data.Word
import Data.Time
import Data.Generics
import Data.Typeable

import Happstack.Data
import Happstack.Data.Serialize
deriving instance Data UTCTime
deriving instance Data Day

instance Data DiffTime where


conDiffTime :: Rational -> DiffTime  
conDiffTime = fromRational

-- toRational  fromRational 
  
data Token =
  Token {
    token :: Word64,
    creation :: UTCTime,
    transmitted :: Maybe UTCTime,
    fetched :: Maybe UTCTime
    } deriving (Read, Show, Eq, Typeable, Data)
               
instance Version Token
$(deriveSerialize ''Token)
               

data TokenPair = 
  TokenPair {
    token_a :: Token,
    token_b :: Token
    } deriving (Read, Show, Eq, Typeable, Data)
               
instance Version TokenPair
$(deriveSerialize ''TokenPair)






