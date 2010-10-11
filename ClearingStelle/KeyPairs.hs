{-# LANGUAGE FlexibleInstances,
             TemplateHaskell,
             StandaloneDeriving,
             DeriveDataTypeable,
             UndecidableInstances,
             Generics #-}

module ClearingStelle.KeyPairs (Identity, Key, KeyPair, randomKey
                               ,mkKeyPair, checkoutKeyPair, disableKeyPair
                               ,keyParser, keyA, keyB
                               ,keyPairDisabled, keyPairCheckedOut) where

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


class (Show k) => Key k where
  randomKey :: IO k
  keyParser :: ReadP k

instance (Key k) => Read k where
  readsPrec _ = readP_to_S $ keyParser
               
data KeyPairEventT = Create | Checkout | Disable
                   deriving (Eq, Show, Typeable, Data)
instance Version KeyPairEventT
$(deriveSerialize ''KeyPairEventT)
                            

data KeyPairEvent = KeyPairEvent KeyPairEventT ClockTime Identity
                     deriving (Data, Typeable, Eq, Show)
instance Version KeyPairEvent
$(deriveSerialize ''KeyPairEvent)
                           
newtype (Key a, Key b) =>
        KeyPair a b = KeyPair (Integer, a, b, [KeyPairEvent])
                    deriving (Eq, Data, Typeable, Show)
instance Version (KeyPair a b)
$(deriveSerialize ''KeyPair)


keyPairEvent :: (Key a, Key b) =>
                     KeyPairEventT -> KeyPair a b -> Bool
keyPairEvent _ (KeyPair (id, _, _, [])) =
                        error $ "KeyPair with id " ++ show id
                        ++ " has empty event queue"
keyPairEvent e (KeyPair (_, _, _, es)) =
  isJust $ find (\(KeyPairEvent t _ _) -> t == e) es



--
-- keyPairCheckedOut returns true if KeyPair already has been checked out
--
keyPairCheckedOut :: (Key a, Key b) =>
                     KeyPair a b -> Bool
keyPairCheckedOut = keyPairEvent Checkout

--
-- keyPairDisabled returns true if KeyPair has been disabled
--
keyPairDisabled :: (Key a, Key b) =>
                     KeyPair a b -> Bool
keyPairDisabled = keyPairEvent Disable

--
-- mkKeyPair creates a new KeyPair with Identity i and the key pair id
--
mkKeyPair :: (Key a, Key b) =>
             Identity -> Integer -> IO (KeyPair a b)
mkKeyPair i id = do
  a <- randomKey
  b <- randomKey
  t <- getClockTime
  return $ KeyPair (id, a, b, [KeyPairEvent Create t i])


--
-- disableKeyPair disables a keypair with identity i
-- multiple calls of disableKeyPair on the same keyapir are allowed
disableKeyPair :: (Key a, Key b) =>
                  Identity -> KeyPair a b -> IO (KeyPair a b)
disableKeyPair i (KeyPair (id, a, b, es)) = do
  t <- getClockTime
  return $ KeyPair (id, a, b, (KeyPairEvent Disable t i) : es )
  
--
-- checkoutKeyPair does a checkout
-- fails on disabled an already checked out keys
--
checkoutKeyPair :: (Key a, Key b) =>
                  Identity -> KeyPair a b -> IO (KeyPair a b)
checkoutKeyPair i kp@(KeyPair (id, a, b, es)) = do
  t <- getClockTime
  let d = keyPairDisabled kp
  let c = keyPairCheckedOut kp
  if d then fail $ "key pair " ++ show id ++ " is disabled. can not checkout"
    else if c then fail $ "key pair " ++ show id ++
                   " has already been checked out."
         else return $ KeyPair (id, a, b, (KeyPairEvent Checkout t i) : es )


--
-- get key a
--
keyA :: (Key a, Key b) => KeyPair a b -> a
keyA (KeyPair (_, a, _, _)) = a

--
-- get key b
--
keyB :: (Key a, Key b) => KeyPair a b -> b
keyB (KeyPair (_, _, b, _)) = b
