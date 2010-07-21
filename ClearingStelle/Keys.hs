{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fglasgow-exts -XNoMonomorphismRestriction #-}


module ClearingStelle.Keys () where


import System.Random
import Data.Generics(Data)
import Data.Typeable
import Data.Char        

import ClearingStelle.Utils

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
  