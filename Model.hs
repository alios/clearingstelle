{-# LANGUAGE Rank2Types, TypeSynonymInstances, QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, GADTs, FlexibleContexts #-}
module Model where

import Yesod
import Database.Persist.GenericSql
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Time.Clock(getCurrentTime, UTCTime(..))
import Data.Text (Text)
import Model.Keys
import Control.Monad.IO.Class
import Yesod.Auth.HashDB (HashDBUser(..))
import Prelude  

data RoleType = AdminRole | DomainAdminRole | InvSideRole | RefSideRole 
              deriving (Show, Read, Enum, Eq)                   
derivePersistField "RoleType"

type ReferenceKey = KeyT LQFBReferenceKey
derivePersistField "ReferenceKey"

type InviteKey = KeyT LQFBInviteKey
derivePersistField "InviteKey"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFile "config/models")

type Database a = (MonadBaseControl IO m, Control.Monad.IO.Class.MonadIO m) => 
                  SqlPersist m a
  
userInRole :: UserId -> RoleType -> DomainId -> Database Bool
userInRole uid role domid = do
  r <- selectFirst [RoleUser ==. uid,
                    RoleType ==. role,
                    RoleDomain ==. domid] [LimitTo 1]
  return $ isJust r

selectDomain :: Text -> Database (Maybe DomainId)
selectDomain dom = do
  val <- selectFirst [DomainName ==. dom] [LimitTo 1]
  return $ maybe Nothing (Just . fst) val

insertKeyset :: DomainId -> Text -> Int -> UserId -> Database KeysetId
insertKeyset dom name n uid = do
  t <- liftIO getCurrentTime
  insert $ Keyset dom name n uid t False False
  
activateKeyset :: KeysetId -> Database ()
activateKeyset set = update set [KeysetActive =. True]

deactivateKeyset :: KeysetId -> Database ()
deactivateKeyset set = update set [KeysetActive =. False]

keysetStats :: KeysetId -> Database (Maybe (KeysetId, (Int, Int)))
keysetStats set = do
  kset' <- get set
  case (kset') of
    Nothing -> return Nothing
    Just kset -> do
      ks <- selectList [KeypairKeyset ==. set] []
      return $ Just (set, (keysetSize kset, length ks))
  
selectIncompleteKeysets :: Database [KeysetId]
selectIncompleteKeysets = do
  ksets' <- selectList [KeysetImported ==. False] []
  let ksets = map fst ksets'
  stats <- sequence $ map keysetStats ksets
  return $ (map $ fst . fromJust) $ filter statsFilter stats
    where statsFilter Nothing = False
          statsFilter (Just (_, (s, l))) = l < s
            
completeKeyset :: KeysetId -> Database Int
completeKeyset set = do
  stats <- keysetStats set
  case stats of
    Nothing -> return 0
    Just (_, (s, l)) -> do
      k <- getJust set
      let dom = keysetDomain k
      ids <- sequence $ replicate (s-l) $ insertRandomKeyPair dom set
      activateKeyset set
      return $ length ids

completeKeysets :: Database Int
completeKeysets = do
  iks <- selectIncompleteKeysets
  r <- sequence $ map completeKeyset iks
  return $ sum r
  
uniqueRefKey :: DomainId -> Database ReferenceKey
uniqueRefKey dom = do
  key <- liftIO randomKey
  key' <- getBy $ UniqueRefKey dom $ key
  case key' of
    Nothing -> return key
    Just _ -> uniqueRefKey dom

uniqueInvKey :: DomainId -> Database InviteKey
uniqueInvKey dom = do
  key <- liftIO randomKey
  key' <- getBy $ UniqueInvKey dom $ key
  case key' of
    Nothing -> return key
    Just _ -> uniqueInvKey dom

randomUniqueKeyPair :: DomainId -> Database (ReferenceKey, InviteKey)
randomUniqueKeyPair d = do
  refKey <- uniqueRefKey d
  invKey <- uniqueInvKey d
  return (refKey, invKey)
  
insertKeyPair :: DomainId -> 
                 KeysetId -> 
                 ReferenceKey -> 
                 InviteKey -> 
                 UTCTime -> 
                 Maybe UTCTime -> 
                 Maybe UTCTime -> 
                 Maybe UserId ->
                 Database KeypairId

insertKeyPair dom set refKey invKey creation checkout deaktivated deactivatedBy  =
  let kp = Keypair dom set refKey invKey creation checkout deaktivated deactivatedBy
  in insert kp

insertRandomKeyPair :: DomainId -> KeysetId -> Database KeypairId
insertRandomKeyPair dom set = do
  (refKey, invKey) <- randomUniqueKeyPair dom
  t <- liftIO getCurrentTime
  insertKeyPair dom set refKey invKey t Nothing Nothing Nothing

deactivateKeys :: UserId -> [ReferenceKey] -> Database [Maybe (InviteKey, Bool)]
deactivateKeys uid rks = sequence $ map (deactivateKey uid) rks

deactivateKey :: UserId -> ReferenceKey -> Database (Maybe (InviteKey, Bool))
deactivateKey uid refKey = do
  kp' <- selectFirst [KeypairRefKey ==. refKey] []
  case (kp') of
    Nothing -> return Nothing
    Just (kpid, kp) -> do
      let invKey = keypairInvKey kp
      let checkout = isJust $ keypairCheckout kp
      let deactive = isJust $ keypairDeactivated kp
      if (not deactive) 
        then do t <- liftIO getCurrentTime
                update kpid [KeypairDeactivated =. (Just t)
                            ,KeypairDeactivatedBy =. (Just uid)]
        else return ()
      return $ Just $ (invKey, checkout)
      
checkoutKey :: DomainId -> ReferenceKey -> Database (Maybe InviteKey)
checkoutKey dom refKey = do
  k' <- getBy $ UniqueRefKey dom refKey
  case k' of
    Nothing -> return Nothing
    Just (kid, k) -> do
      let set' = (keypairKeyset k)
      set <- getJust set'
      if (or [isJust $ keypairCheckout k
             ,isJust $ keypairDeactivated k 
             ,not $ keysetActive set]) 
        then return Nothing
        else do t <- liftIO getCurrentTime
                update kid [KeypairCheckout =. (Just t)]
                return $ Just $ keypairInvKey k
                
  
cleanupKeys :: DomainId -> [InviteKey] -> Database [Maybe ReferenceKey]
cleanupKeys dom invKeys = sequence $ map (cleanupKey dom) invKeys

cleanupKey :: DomainId -> InviteKey -> Database (Maybe ReferenceKey)
cleanupKey dom invKey = do
  key' <- getBy $ UniqueInvKey dom invKey
  case key' of
    Nothing -> return Nothing
    Just (_, key) -> do
      return $ Just $ keypairRefKey key
      
      

instance HashDBUser (UserGeneric b) where
  userPasswordHash = userPassword
  userPasswordSalt = userSalt
  setSaltAndPasswordHash s h u = u { userSalt     = Just s
                                   , userPassword = Just h
                                   }

