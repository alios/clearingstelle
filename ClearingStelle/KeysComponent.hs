{-# LANGUAGE TemplateHaskell, 
             StandaloneDeriving, 
             DeriveDataTypeable, 
             Generics, 
             TypeFamilies #-}

module ClearingStelle.KeysComponent ( nextKeyId, createKPs
                                    , disableKP, checkoutKP
                                    , findByKeyA, findByKeyB ) where

import Data.List (find)
import Data.Typeable
import Data.Data
import Control.Monad.Trans

import Happstack.Data
import Happstack.State

import ClearingStelle.KeyPairs
import ClearingStelle.Keys

data (Key a, Key b) => KeysComponent a b =
  KeysComponent {
    next_id :: Integer,
    keys :: [KeyPairT a b]
    } deriving (Show, Data, Typeable)
                   
instance (Key a, Key b) => Version (KeysComponent a b)
$(deriveSerialize ''KeysComponent)

instance (Key a, Key b) => Component (KeysComponent a b) where
  type Dependencies (KeysComponent a b) = End
  initialValue = KeysComponent 0 []
  

nextKeyId :: (Key a, Key b) => Query (KeysComponent a b) Integer
nextKeyId = fmap next_id askState
  
findByKeyA :: (Key a, Key b) => 
              a -> Query (KeysComponent a b) (Maybe (KeyPairT a b))
findByKeyA a = do
  ks <- fmap keys askState
  return $ find (\kp -> a == keyA kp) ks 

findByKeyB :: (Key a, Key b) => 
              b -> Query (KeysComponent a b) (Maybe (KeyPairT a b))
findByKeyB b = do
  ks <- fmap keys askState
  return $ find (\kp -> b == keyB kp) ks 

updateKey :: (Key a, Key b) => 
             (KeyPairT a b) -> Update (KeysComponent a b) ()
updateKey kp = do
  keys <- fmap keys getState
  nid <- runQuery nextKeyId
  let keys' = filter (\kp' -> keyId kp /= keyId kp') keys
  if ((length keys) - (length keys') == 1)
    then putState $ KeysComponent nid (kp :keys')
    else fail $ "unable to find KeyPair with id " ++ (show $ keyId kp)

createKPs :: (Key a, Key b) =>
                  Identity -> Integer -> Update (KeysComponent a b) Integer
createKPs id n = do
  next_id <- runQuery nextKeyId
  old_ks <- fmap keys getState
  ks <- unsafeIOToEv $ mkKeyPairs old_ks id next_id n
  putState $ KeysComponent (next_id + n) (ks ++ old_ks)
  return n

disableKP :: (Key a, Key b) =>
                  Identity -> a -> Update (KeysComponent a b) ()
disableKP id a = do
  mkp <- runQuery $ findByKeyA a
  case mkp of
    Nothing -> fail $ "unable to find Key with key a " ++ show a 
    Just kp -> do 
      kp' <- unsafeIOToEv $  disableKeyPair id kp
      updateKey kp'


checkoutKP :: (Key a, Key b) =>
                  Identity -> a -> Update (KeysComponent a b) b
checkoutKP id a = do
  mkp <- runQuery $ findByKeyA a
  case mkp of
    Nothing -> fail $ "unable to find Key with key a " ++ show a 
    Just kp -> do 
      kp' <- unsafeIOToEv $  checkoutKeyPair id kp
      updateKey kp'
      return $ keyB kp'



