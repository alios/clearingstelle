{-# LANGUAGE TemplateHaskell, 
             StandaloneDeriving, 
             DeriveDataTypeable, 
             Generics #-}

module ClearingStelle.KeyPairs (Identity, KeyPair
                               , mkKeyPair, checkoutKeyPair, disableKeyPair
                               ,keyPairDisabled, keyPairCheckedOut) where

import System.Random

import Data.Maybe
import Data.List
import Data.Typeable
import Data.Data
import Happstack.Data

import Data.Time.Clock
                           

type Identity = String

data KeyPairEventT = Create | Checkout | Disable
                   deriving (Eq, Show, Typeable, Data)
instance Version KeyPairEventT
$(deriveSerialize ''KeyPairEventT)  
                            

data KeyPairEvent = KeyPairEvent KeyPairEventT UTCTime Identity
                     deriving (Data, Typeable, Eq, Show)
instance Version KeyPairEvent
$(deriveSerialize ''KeyPairEvent)
                           
newtype (Bounded a, Bounded b, Random a, Random b) => 
        KeyPair a b = KeyPair (Integer, a, b, [KeyPairEvent])
                    deriving (Eq, Data, Typeable, Show)
instance Version (KeyPair a b)
$(deriveSerialize ''KeyPair)


keyPairEvent :: (Bounded a, Bounded b, Random a, Random b) => 
                     KeyPairEventT -> KeyPair a b -> Bool
keyPairEvent _ (KeyPair (id, _, _, [])) = 
                        error $ "KeyPair with id " ++ show id 
                        ++ " has empty event queue"
keyPairEvent e (KeyPair (_, _, _, es)) =
  isJust $ find (\(KeyPairEvent t _ _) -> t == e) es



--
-- keyPairCheckedOut returns true if KeyPair already has been checked out
--
keyPairCheckedOut :: (Bounded a, Bounded b, Random a, Random b) => 
                     KeyPair a b -> Bool
keyPairCheckedOut = keyPairEvent Checkout

--
-- keyPairDisabled returns true if KeyPair has been disabled
--
keyPairDisabled :: (Bounded a, Bounded b, Random a, Random b) => 
                     KeyPair a b -> Bool
keyPairDisabled = keyPairEvent Disable                        

--
-- mkKeyPair creates a new KeyPair with Identity i and the key pair id
--
mkKeyPair :: (Bounded a, Bounded b, Random a, Random b) => 
             Identity -> Integer -> IO (KeyPair a b)
mkKeyPair i id = do  
  a <- getStdRandom (randomR (minBound, maxBound))
  b <- getStdRandom (randomR (minBound, maxBound))
  t <- getCurrentTime
  return $ KeyPair (id, a, b, [KeyPairEvent Create t i])


--
-- disableKeyPair disables a keypair with identity i 
--  multiple calls of disableKeyPair on the same keyapir are allowed
disableKeyPair :: (Bounded a, Bounded b, Random a, Random b) => 
                  Identity -> KeyPair a b -> IO (KeyPair a b)
disableKeyPair i (KeyPair (id, a, b, es)) = do
  t <- getCurrentTime
  return $ KeyPair (id, a, b, (KeyPairEvent Disable t i) : es )
  
--
-- checkoutKeyPair does a checkout
-- fails on disabled an already checked out keys
--
checkoutKeyPair :: (Bounded a, Bounded b, Random a, Random b) => 
                  Identity -> KeyPair a b -> IO (KeyPair a b)
checkoutKeyPair i kp@(KeyPair (id, a, b, es)) = do
  t <- getCurrentTime
  let d = keyPairDisabled kp
  let c = keyPairCheckedOut kp
  if d then fail $ "key pair " ++ show id ++ " is disabled. can not checkout"
    else if c then fail $ "key pair " ++ show id ++ 
                   " has already been checked out."
         else return $ KeyPair (id, a, b, (KeyPairEvent Checkout t i) : es )

-- this function is for testing only
mkIntKeyPair :: Identity -> Integer -> IO (KeyPair Int Int)
mkIntKeyPair i id = mkKeyPair i id


