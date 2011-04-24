{-
Copyright (c)2011, Markus Barenhoff <alios@alios.org>

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Markus Barenhoff nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE QuasiQuotes, 
             TypeFamilies, 
             GeneralizedNewtypeDeriving, 
             TemplateHaskell, 
             OverloadedStrings, 
             NoMonomorphismRestriction #-}

module KeysDB ( migrateAll, mkKeyPair, checkoutKey, disableKey
              , KeyPairId, keyPairRefkey, keyPairInvkey
              , keyPairCreated, keyPairCheckedOut, keyPairDisabled)  where
import Database.Persist.TH  
import Database.Persist
import Data.Time 
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Class (MonadIO)
import Keys

share2 mkPersist (mkMigrate "migrateAll") [persist|
KeyPair
  refkey RefKey Eq Unique
  invkey InviteKey Eq Unique
  created UTCTime default=CURRENT_TIMESTAMP
  checkedOut UTCTime Maybe Eq Update default=Nothing
  disabled UTCTime Maybe Eq Update default=Nothing
|]

disableKey :: (PersistBackend m, MonadIO m) => RefKey -> m ()  
disableKey rk = do
  t <- liftIO getCurrentTime
  updateWhere 
    [KeyPairRefkeyEq rk, KeyPairDisabledEq Nothing] 
    [KeyPairDisabled $ Just t]

checkoutKey :: (PersistBackend m, MonadIO m) => RefKey -> m (Maybe InviteKey)
checkoutKey rk = do
  kp <- selectList (refKeyAvail rk) [] 1 0
  if (null kp) then return Nothing else do
    t <- liftIO getCurrentTime
    updateWhere (refKeyAvail rk) [KeyPairCheckedOut $ Just t]
    return $ Just $ keyPairInvkey $ snd $ kp !! 0
  where  
    refKeyAvail r = 
      [ KeyPairRefkeyEq r, 
        KeyPairCheckedOutEq Nothing, 
        KeyPairDisabledEq Nothing]
      
mkKeyPair :: (MonadIO m, PersistBackend m) => m (Key KeyPair)
mkKeyPair = do
  r <- uniqRefKey
  i <- uniqInviteKey
  t <- liftIO getCurrentTime
  insert $ KeyPair r i t Nothing Nothing
  where uniqRefKey = do
          rk <- liftIO randomRefKey
          rkc <- count [KeyPairRefkeyEq rk]
          if (rkc == 0) then return rk else uniqRefKey
        uniqInviteKey = do
          ik <- liftIO randomInviteKey
          ikc <- count [KeyPairInvkeyEq ik]
          if (ikc == 0) then return ik else uniqInviteKey

