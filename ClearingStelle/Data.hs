{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fglasgow-exts -XDeriveDataTypeable -XStandaloneDeriving #-}


module ClearingStelle.Data where

import Data.Word
import Data.Time
import Data.Generics
import Data.Typeable

import Happstack.Data
import Happstack.State

import System.Random

deriving instance Data Day


data TimeStampUTC = 
  TimeStampUTC Day Rational   
  deriving (Read, Show, Eq, Typeable, Data)
                           
instance Version TimeStampUTC

$(deriveSerialize ''TimeStampUTC)



timestamp2UtcTime :: TimeStampUTC -> UTCTime
timestamp2UtcTime (TimeStampUTC day diff) = 
  let diff' = conDiffTime diff
  in UTCTime day diff'
     
utcTime2Timestamp :: UTCTime -> TimeStampUTC
utcTime2Timestamp (UTCTime day diff) =
  let diff' = toRational diff
  in TimeStampUTC day diff'
     
conDiffTime :: Rational -> DiffTime  
conDiffTime = fromRational
  
data TokenT =
  Token {
    token :: Word64,
    creation :: TimeStampUTC,
    transmitted :: Maybe TimeStampUTC,
    fetched :: Maybe TimeStampUTC
    } deriving (Read, Show, Eq, Typeable, Data)


instance Version TokenT
$(deriveSerialize ''TokenT)
               


data TokenPairT = 
  TokenPair {
    token_a :: TokenT,
    token_b :: TokenT
    } deriving (Read, Show, Eq, Typeable, Data)
               
instance Version TokenPairT 
         
$(deriveSerialize ''TokenPairT)

data AppState =
  AppState {
    stateTokenSets :: [[ TokenPairT ]]
    } deriving (Read, Show, Eq, Typeable, Data)
               
instance Version AppState
$(deriveSerialize ''AppState)

instance Component AppState where
  type Dependencies AppState = End
  initialValue = AppState { stateTokenSets = [] }
  
  
getRandom' rs = do  
  token <- getStdRandom (randomR (minBound, maxBound))
  if (elem token rs) 
    then getRandom' rs
    else return token
  
genRandomToken :: [TokenT] -> IO TokenT
genRandomToken ts = 
  let ts' = map token ts
  in do
    token <- getRandom' ts'
    ts <- fmap utcTime2Timestamp getCurrentTime
    return $ Token token ts Nothing Nothing
  
genRandomTokenPair :: [TokenT] -> IO TokenPairT
genRandomTokenPair ts = do
  tokenA <- genRandomToken ts
  tokenB <- genRandomToken $ tokenA : ts
  return $ TokenPair tokenA tokenB



genRandomTokenPairs :: Integer -> IO [TokenPairT]
genRandomTokenPairs = genRandomTokenPairs' []

genRandomTokenPairs' ts n
      | (n <= 0)  = return []
      | otherwise = do
        t <- genRandomTokenPair ts
        let tokenA = token_a t
        let tokenB = token_b t
        ts <- genRandomTokenPairs' (tokenA : tokenB :ts) (n - 1)
        return $ t : ts