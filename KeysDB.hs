{-# LANGUAGE QuasiQuotes, 
             TypeFamilies, 
             GeneralizedNewtypeDeriving, 
             TemplateHaskell, 
             OverloadedStrings, 
             NoMonomorphismRestriction #-}


module KeysDB  where
  
import Database.Persist
import Database.Persist.TH
import Data.Time 
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Class (MonadIO)
import Keys

derivePersistField "RefKey"
derivePersistField "InviteKey"


share2 mkPersist (mkMigrate "migrateAll") [persist|
KeyPair
  refkey RefKey Eq 
  invkey InviteKey Eq
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
    refKeyAvail rk = 
      [ KeyPairRefkeyEq rk, 
        KeyPairCheckedOutEq Nothing, 
        KeyPairDisabledEq Nothing]

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

