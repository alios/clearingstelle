{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fglasgow-exts -XNoMonomorphismRestriction #-}


module ClearingStelle.Keys (KeyStore(..)
                           , manager_createkeyset_get
                           , manager_createkeyset_post
                           , inviteSite_getkeys, reqSite_getkeys) where

import System.Random
import Data.Generics(Data)
import Data.Typeable
import Data.Char        
import Data.Time
import Data.Maybe
import Data.List
import Text.XHtml.Strict as XHTML

import Control.Monad.State 
import Control.Monad.Reader
import Happstack.Data
import Happstack.State
import Happstack.Server.SimpleHTTP


import ClearingStelle.UserDB
import ClearingStelle.Utils

newtype Tupel = Tupel String
                deriving (Eq, Data, Typeable)
instance Version Tupel  
$(deriveSerialize ''Tupel)

instance Show Tupel where
    show (Tupel s) = s




data InviteKey = InviteKey (Tupel, Tupel, Tupel, Tupel)
               deriving (Eq, Data, Typeable)
instance Version InviteKey
$(deriveSerialize ''InviteKey)
                        
instance Show InviteKey where
    show (InviteKey (a,b,c,d)) =
        let ts = [show t | t <- [a,b,c,d]]
        in intercalate "-" ts

inviteKeyTupelLen = 5

data RefKey = RefKey (Tupel, Tupel, Tupel, Tupel, Tupel)
               deriving (Eq, Data, Typeable)
instance Version RefKey
$(deriveSerialize ''RefKey)

instance Show RefKey where
    show (RefKey (a,b,c,d,e)) =
        let ts = [show t | t <- [a,b,c,d,e]]
        in intercalate "-" ts

refKeyTupelLen = 4



data KeyPair = KeyPair {
      kp_inviteKey :: InviteKey,
      kp_refKey :: RefKey,
      kp_created :: UTCTime,
      kp_inviteKeyFetched :: Maybe UTCTime,
      kp_refKeyFetched :: Maybe UTCTime,
      kp_checkedOut :: Maybe UTCTime
} deriving (Show, Data, Typeable)
instance Version KeyPair
$(deriveSerialize ''KeyPair)


data KeySet = KeySet {
      ks_name :: String,
      ks_keyPairs :: [KeyPair],
      ks_created :: UTCTime,
      ks_manager :: User,
      ks_inviteSite :: User,
      ks_requestSite :: User,
      ks_notifyInviteSite :: Bool,
      ks_notifyRequestSite :: Bool,
      ks_disabled :: Bool
} deriving (Show, Data, Typeable)
instance Version KeySet

$(deriveSerialize ''KeySet)

data KeyStore = KeyStore {
      keySets :: [KeySet]
} deriving (Show, Data, Typeable)

instance Version KeyStore

$(deriveSerialize ''KeyStore)

instance Component KeyStore where
    type Dependencies KeyStore = End
    initialValue = KeyStore []

-- | 'validChar' is a predicate for the allowed characters in the 'RefKEy' and
--   in the 'InviteKey'.
--   allowed are all lower case ASCII characters and digits exept all vocals and
--   the following: '0', '1', 'l'
validChar :: Char -> Bool
validChar c =
  (isAlphaNum c) && 
  (isDigit c || isAsciiLower c) &&
  (notElem c invalidChars) 
    where invalidChars = "aeiou01l"
  

validChars :: [Char]
validChars = [ c | c <- map chr [0..255], validChar c]

getRandomChar = do  
  i <- getStdRandom (randomR (minBound, maxBound))
  return $ validChars !! (i `mod` length validChars)
         
getRandomTupel :: Int -> IO Tupel
getRandomTupel len = fmap Tupel $ getRandomTupel' len
getRandomTupel' len
  | (len < 0)  = error $ "len must not be negative: " ++ show len
  | (len == 0) = return []
  | otherwise  = do
    char <- getRandomChar
    chars <- getRandomTupel' (len - 1)
    return $ char : chars

getRandomInviteKey :: IO InviteKey
getRandomInviteKey = do
  a <- getRandomTupel inviteKeyTupelLen
  b <- getRandomTupel inviteKeyTupelLen
  c <- getRandomTupel inviteKeyTupelLen
  d <- getRandomTupel inviteKeyTupelLen
  return $ InviteKey (a,b,c,d)
  
getRandomRefKey :: IO RefKey
getRandomRefKey = do
  a <- getRandomTupel refKeyTupelLen
  b <- getRandomTupel refKeyTupelLen
  c <- getRandomTupel refKeyTupelLen
  d <- getRandomTupel refKeyTupelLen
  e <- getRandomTupel refKeyTupelLen  
  return $ RefKey (a,b,c,d,e)

getRandomKeyPair :: IO KeyPair
getRandomKeyPair = do
  i <- getRandomInviteKey
  r <- getRandomRefKey
  cd <- getCurrentTime
  return $ KeyPair i r cd Nothing Nothing Nothing

getRandomKeyPairs :: [KeyPair] -> Int -> IO [KeyPair]
getRandomKeyPairs ps n
  | (n < 0) = fail $ "n must not be negative: " ++ show n
  | (n == 0) = return []
  | otherwise = 
    let is = map kp_inviteKey ps 
        rs = map kp_refKey ps
    in do 
      p <- getRandomKeyPair
      let i = kp_inviteKey p
          r = kp_refKey p
      if ((elem i is) || (elem r rs))
        then getRandomKeyPairs ps n
        else do ps' <- getRandomKeyPairs (p:ps) (n - 1)
                return $ p : ps'
        

prop_uniqueKeys :: [KeyPair] -> Bool
prop_uniqueKeys [] = True
prop_uniqueKeys (k:ks) =
  let is = map kp_inviteKey ks
      rs = map kp_refKey ks
      i = kp_inviteKey k
      r = kp_refKey k
  in  (notElem i is) && (notElem r rs) && prop_uniqueKeys ks

-- | 'validTupel' is a predicate for a valid Tupel of a given length 'len'.
--   it checks if the tupel has only valid characters
validTupel :: Int -> Tupel -> Bool
validTupel len (Tupel t) =
  (length t == len) &&
  (and $ map validChar t)
  

validInviteKey (InviteKey (a,b,c,d)) = 
  validKey inviteKeyTupelLen [a,b,c,d]

validRefKey (RefKey (a,b,c,d,e)) = 
  validKey refKeyTupelLen [a,b,c,d,e]

validKey len ts =
  (length ts == len) &&
  (and $ map (validTupel len) ts)

validKeySetName :: [KeySet] -> KeySet -> Bool
validKeySetName [] _ = True
validKeySetName (k:ks) n
    | (ks_name k) == (ks_name n) = False
    | otherwise = validKeySetName ks n


isKeysetsInviteSite :: String -> User -> Query KeyStore Bool
isKeysetsInviteSite n u = do
  is <- fmap (ks_inviteSite . fromJust) $ getKeySet n
  return $ is == u

getKeySet :: String -> Query KeyStore (Maybe KeySet)
getKeySet n = do
  ks <- fmap (filter (\s -> n == ks_name s)) getAllKeySets
  case (ks) of
    []  -> return Nothing
    [s] -> return $ Just s
    otherwise -> error $ "found duplicate keysets with name: " ++ n
  
getKeyPairsFromSet :: String -> Query KeyStore (Maybe [KeyPair])
getKeyPairsFromSet n = do
  ks <- getKeySet n
  case (ks) of 
    Just ks' -> return $ Just $ ks_keyPairs ks'
    Nothing -> return $ Nothing

getInviteKeysFromSet :: String -> Query KeyStore (Maybe [(InviteKey, Bool)])
getInviteKeysFromSet n = do
  kps <- getKeyPairsFromSet n
  case (kps) of 
    Just kps' -> return $ Just [ (kp_inviteKey kp , isNothing $ kp_inviteKeyFetched kp) | kp <- kps' ]
    Nothing -> return Nothing
         
getRefKeysFromSet :: String -> Query KeyStore (Maybe [(RefKey, Bool)])
getRefKeysFromSet n = do
  kps <- getKeyPairsFromSet n
  case (kps) of 
    Just kps' -> return $ Just [ (kp_refKey kp, isNothing $ kp_inviteKeyFetched kp) | kp <- kps' ]
    Nothing -> return Nothing

getAllKeySets :: Query KeyStore [KeySet]
getAllKeySets = fmap  keySets askState

getAllKeyPairs = fmap (concat . (map ks_keyPairs)) getAllKeySets

insertKeySet :: KeySet -> Update KeyStore ()
insertKeySet ks = 
    do kss <- runQuery getAllKeySets
       if (validKeySetName kss ks) then 
           putState $ KeyStore (ks:kss) else 
           fail $ "KeySet with name " ++ ks_name ks ++ " already exists in KeyStore." 
     
createKeySet :: String -> User -> User -> User -> Int -> Bool -> Bool -> Update KeyStore ()
createKeySet name m inv req n notifyi notifyr  =
    do kps' <- runQuery getAllKeyPairs
       kps <- unsafeIOToEv $ getRandomKeyPairs kps' n
       time <- unsafeIOToEv $ getCurrentTime
       insertKeySet $ KeySet name kps time m inv req notifyi notifyr False

$(mkMethods ''KeyStore ['createKeySet, 'getKeySet, 'isKeysetsInviteSite, 'getInviteKeysFromSet, 'getRefKeysFromSet])

inviteSite_getkeys :: String -> ServerPart Response
inviteSite_getkeys name' = do
  let name = tail name'
  user <- fmap fromJust getCurrentUser
  ks <- query $ GetKeySet name 
  case (ks) of
    Nothing -> fail $ "keyset with name " ++ name ++ " couls not be found."
    Just ks -> do
              valid <- query $ IsKeysetsInviteSite name user
              if (valid) then 
                  do keys <- query $ GetInviteKeysFromSet name
                     ok $ toResponse $ show keys else
                  unauthorized $ toResponse "you are not allowed to receive invite keys"

reqSite_getkeys :: String -> ServerPart Response
reqSite_getkeys name' = do
  let name = tail name'
  user <- fmap fromJust getCurrentUser
  ks <- query $ GetKeySet name 
  case (ks) of
    Nothing -> fail $ "keyset with name " ++ name ++ " couls not be found."
    Just ks -> do
              valid <- query $ IsKeysetsInviteSite name user
              if (valid) then 
                  do keys <- query $ GetRefKeysFromSet name
                     ok $ toResponse $ show keys else
                  unauthorized $ toResponse "you are not allowed to receive invite keys"

manager_createkeyset_post :: ServerPart Response
manager_createkeyset_post = do
  user <- fmap fromJust getCurrentUser
  postdata <- getDataFn manager_createkeyset_data

  case postdata of 
    Nothing -> badRequest $ toResponse $ "invalid/insufficient POST data"
    Just (name, inv, ref, n, notifyi, notifyr) -> 
        do iuser <- query $ GetUser inv
           ruser <- query $ GetUser ref
           inv'  <- case iuser of
                     Nothing -> fail $ "unkown invite site:"
                     Just i -> return i
           ref'  <- case ruser of
                      Nothing -> fail $ "unknown ref site:" ++ ref
                      Just r -> return r
           valid <- query $ ValidRoles (user_email inv') (user_email ref')        
           if (valid) then do    
                        update $ CreateKeySet name user inv' ref' n notifyi notifyr
                        ok $ toResponse $ "created keyset" else 
               fail $ show inv ++ " or " ++ " don't have the valid roles."


manager_createkeyset_get = 
    ok $ toResponse $ manager_createkeyset_page
    
manager_createkeyset_data :: RqData (String, String, String, Int, Bool, Bool)
manager_createkeyset_data = do
  req  <- lookPairs
  name <- fmap trim $ look "Name"
  inv  <- fmap trim $ look "InviteKeySite"
  ref  <- fmap trim $ look "RefKeySite"
  n    <- lookRead "Count" 
  let notifyi = isJust $ lookup "InviteKeyNotify" req
      notifyr = isJust $ lookup "RefKeyNotify" req
  return $ (name, inv, ref, n, notifyi, notifyr)
       

manager_createkeyset_page :: Html
manager_createkeyset_page =
    let f = build_form "Create Keyset" "/manager/createkeyset"
            [ ("Parameters", [("Name", textfield "")
                          ,("InviteKeySite", textfield "") 
                          ,("RefKeySite", textfield "")
                          ])
            , ("Options", [("Count", textfield "")
                          ,("InviteKeyNotify", checkbox "notifyi" "notifyi")
                          ,("RefKeyNotify", checkbox "notifyr" "notifyr")
                          ])
            ]
    in page "Create Keyset" f

