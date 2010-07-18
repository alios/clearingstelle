{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fglasgow-exts -XNoMonomorphismRestriction #-}


module ClearingStelle.Data(clearingStelleState, getUserMap, addUser
                          ,roleAuth, UserDB, adminRole, managerRole, inviteSiteRole, requestSiteRole
) where

import Data.Char
import qualified Data.Map as M
import Data.Time
import Data.Generics(Data)
import Data.Typeable

import Control.Monad.State 
import Control.Monad.Reader

import Happstack.Data
import Happstack.State
import Happstack.Server.SimpleHTTP


import System.Random

import Test.QuickCheck



------------------------------ UserDB ------------------------------

data Role = Admin | Manager | InviteSite | RequestSite
          deriving (Show, Read, Eq, Enum, Data, Typeable)
instance Version Role
$(deriveSerialize ''Role)

adminRole = Admin
managerRole = Manager
inviteSiteRole = InviteSite
requestSiteRole = RequestSite



data User = User {
      user_email :: String,
      user_pw :: String,
      user_roles :: [ Role ]
    } deriving (Show, Read, Eq, Data, Typeable)
instance Version User
$(deriveSerialize ''User)


newtype UserDB = UserDB {
      userdb_users :: [ User ]
    } deriving (Show, Read, Eq, Data, Typeable)

instance Version UserDB
$(deriveSerialize ''UserDB)

instance Component UserDB where
    type Dependencies UserDB = End
    initialValue = UserDB [ User "hans@wurst.de" "test" [Admin]]


getUserMap :: Role -> Query UserDB (M.Map String String)
getUserMap r =
    do userdb <- fmap userdb_users ask
       let us = filter (\u -> r `elem` (user_roles u)) userdb
       return $ M.fromList [ (user_email u, user_pw u) | u <- us ]


addUser :: String -> String -> [ Role ] -> Update UserDB ()
addUser u p rs = modify $ (\db -> UserDB $ add_user u p rs $ userdb_users db)
    where add_user u p rs users 
              | null u = fail "username must not be empty"
              | null p = fail "password must not be empty"
              | elem u $ [user_email u | u <- users] = fail $ "user " ++ u ++ " already exists"
              | otherwise = (User u p rs) : users
    

$(mkMethods ''UserDB ['getUserMap, 'addUser])

roleAuth :: Role -> ServerPart Response -> ServerPart Response
roleAuth r p =
    do usermap <- query $ GetUserMap r
       basicAuth "please authorize yourself" usermap p




------------------------------ AppState ------------------------------

data AppState = AppState
                deriving (Show, Read, Eq, Ord, Typeable, Data)
                         
instance Version AppState
$(deriveSerialize ''AppState)


instance Component AppState where
    type Dependencies AppState = UserDB :+: End
    initialValue = AppState

$(mkMethods ''AppState [])

clearingStelleState :: Proxy AppState
clearingStelleState = Proxy


------------------------------ Tupels and Keys ------------------------------

          

newtype Tupel = Tupel String
                deriving (Show, Read, Eq, Data, Typeable)
                         
    
data InviteKey = InviteKey (Tupel, Tupel, Tupel, Tupel)
               deriving (Show, Read, Eq, Data, Typeable)
                        
inviteKeyTupelLen = 5

data RefKey = RefKey (Tupel, Tupel, Tupel, Tupel, Tupel)
               deriving (Show, Read, Eq, Data, Typeable)

refKeyTupelLen = 4

type KeyPair = (InviteKey, RefKey)



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
  let char = validChars !! (i `mod` length validChars)
  if (validChar char) 
    then return char
    else getRandomChar
         
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
  a <- getRandomTupel inviteKeyTupelLen
  b <- getRandomTupel inviteKeyTupelLen
  c <- getRandomTupel inviteKeyTupelLen
  d <- getRandomTupel inviteKeyTupelLen
  e <- getRandomTupel inviteKeyTupelLen  
  return $ RefKey (a,b,c,d,e)

getRandomKeyPair :: IO KeyPair
getRandomKeyPair = do
  i <- getRandomInviteKey
  r <- getRandomRefKey
  return $ (i,r)

getRandomKeyPairs :: [KeyPair] -> Int -> IO [KeyPair]
getRandomKeyPairs ps n
  | (n < 0) = fail $ "n must not be negative: " ++ show n
  | (n == 0) = return []
  | otherwise = 
    let is = map fst ps 
        rs = map snd ps
    in do 
      p@(i, r) <- getRandomKeyPair
      if ((elem i is) || (elem r rs))
        then getRandomKeyPairs ps n
        else do ps' <- getRandomKeyPairs (p:ps) (n - 1)
                return $ p : ps'
        

prop_uniqueKeys :: [KeyPair] -> Bool
prop_uniqueKeys [] = True
prop_uniqueKeys ((i,r):ks) =
  let is = map fst ks
      rs = map snd ks
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
  
