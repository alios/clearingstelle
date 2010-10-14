{-# LANGUAGE FlexibleInstances,
             TemplateHaskell,
             StandaloneDeriving,
             DeriveDataTypeable,
             UndecidableInstances,
             MultiParamTypeClasses,
             Generics #-}

module ClearingStelle.KeyPairs (Identity, Key, KeyPairT, randomKey, keyParser
                               ,mkKeyPair, checkoutKeyPair, disableKeyPair
                               ,keyA, keyB, keyId, keyPairState
                               ,mkKeyPairs, keysUsed) where

import System.Random
import System.Time

import Data.Maybe
import Data.List
import Data.Typeable
import Data.Data
import Happstack.Data
import Text.ParserCombinators.ReadP                           

deriving instance Data ClockTime
deriving instance Typeable ClockTime
instance Version ClockTime
$(deriveSerialize ''ClockTime)
 
type Identity = String

class (Show k, Eq k, Serialize k) => Key k where
  randomKey :: IO k
  keyParser :: ReadP k

instance (Key k) => Read k where
  readsPrec _ = readP_to_S $ keyParser
               
data KeyPairState = Created | Checkedout | Disabled
                   deriving (Eq, Show, Typeable, Data)
instance Version KeyPairState
$(deriveSerialize ''KeyPairState)
                            

data KeyPairEvent = KeyPairEvent KeyPairState ClockTime Identity
                     deriving (Data, Typeable, Eq, Show)
                              
instance Version KeyPairEvent
$(deriveSerialize ''KeyPairEvent)
                           
newtype (Key a, Key b) =>
        KeyPairT a b = KeyPair (Integer, a, b, [KeyPairEvent])
                    deriving (Eq, Data, Typeable, Show)
instance Version (KeyPairT a b)
$(deriveSerialize ''KeyPairT)

keyPairEvent :: (Key a, Key b) =>
                     KeyPairState -> KeyPairT a b -> Bool
keyPairEvent _ (KeyPair (id, _, _, [])) =
                        error $ "KeyPairT with id " ++ show id
                        ++ " has empty event queue"
keyPairEvent e (KeyPair (_, _, _, es)) =
  isJust $ find (\(KeyPairEvent t _ _) -> t == e) es

keyPairState :: (Key a, Key b) =>
                     KeyPairT a b -> KeyPairState
keyPairState kp
  | (keyPairEvent Disabled kp) = Disabled
  | (keyPairEvent Checkedout kp) = Checkedout
  | otherwise = Created

-- | creates a new KeyPairT with Identity i and the key pair id
mkKeyPair :: (Key a, Key b) =>
             Identity -> Integer -> IO (KeyPairT a b)
mkKeyPair i id = do
  a <- randomKey
  b <- randomKey
  t <- getClockTime
  return $ KeyPair (id, a, b, [KeyPairEvent Created t i])



-- | make a list of n random KeyPairs starting from id start.
-- the as and the bs are unique across the returned list
mkKeyPairs :: (Key a, Key b) => 
              [KeyPairT a b] -> Identity -> Integer -> Integer -> 
              IO [KeyPairT a b]
mkKeyPairs = mkKeyPairs' [] 
  where mkKeyPairs' new old i start n
          | (n <= 0) = do return new
          | otherwise = do
            key <- mkKeyPair i start
            if (keysUsed (new ++ old) key)
              then mkKeyPairs' new old i start n
              else mkKeyPairs' (new ++ [key]) old i (succ start) (pred n)  
            

-- | disableKeyPair disables a keypair with identity i
-- multiple calls of disableKeyPair on the same keyapir are allowed
disableKeyPair :: (Key a, Key b) =>
                  Identity -> KeyPairT a b -> IO (KeyPairT a b)
disableKeyPair i (KeyPair (id, a, b, es)) = do
  t <- getClockTime
  return $ KeyPair (id, a, b, (KeyPairEvent Disabled t i) : es )
  

-- | checkoutKeyPair does a checkout
-- fails on disabled an already checked out keys
checkoutKeyPair :: (Key a, Key b) =>
                  Identity -> KeyPairT a b -> IO (KeyPairT a b)
checkoutKeyPair i kp@(KeyPair (id, a, b, es)) = do
  t <- getClockTime
  let state = keyPairState kp
  case state of 
    Disabled -> 
      fail $ "key pair " ++ show id ++ " is disabled. can not checkout"
    Checkedout -> 
      fail $ "key pair " ++ show id ++ " has already been checked out."
    Created -> 
      return $ KeyPair (id, a, b, (KeyPairEvent Checkedout t i) : es )


-- | get key a
keyA :: (Key a, Key b) => KeyPairT a b -> a
keyA (KeyPair (_, a, _, _)) = a

-- | get key b
keyB :: (Key a, Key b) => KeyPairT a b -> b
keyB (KeyPair (_, _, b, _)) = b

-- | get key id
keyId ::(Key a, Key b) => KeyPairT a b -> Integer
keyId (KeyPair (id, _, _, _)) = id

-- | returns if either the keyA or the keyB or the id of the given KeyPairT 
-- exists in the given list of keyPairs
keysUsed :: (Key a, Key b) => [KeyPairT a b] -> KeyPairT a b -> Bool 
keysUsed [] _ = False
keysUsed (k:ks) x = 
  let ka = keyA k == keyA x
      kb = keyB k == keyB x
      ki = keyId k == keyId x
      rst = keysUsed ks x
  in ka || kb || ki || rst
     
