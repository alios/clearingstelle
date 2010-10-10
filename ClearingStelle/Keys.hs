{-# LANGUAGE TemplateHaskell, 
             StandaloneDeriving, 
             DeriveDataTypeable, 
             Generics #-}

module ClearingStelle.Keys where

import System.Random
import Data.Typeable
import Data.Data
import Data.Char        
import Data.Maybe
import Data.List
import Text.ParserCombinators.Parsec hiding (getState)
import Happstack.Data

import ClearingStelle.KeyPairs

refKeyTupelLen = 4
inviteKeyTupelLen = 5

--
-- Tupel
--
newtype Tupel = Tupel String
                deriving (Eq, Data, Typeable)
instance Version Tupel  
$(deriveSerialize ''Tupel)

instance Show Tupel where
    show (Tupel s) = s

--
-- InviteKey
--
data InviteKey = InviteKey (Tupel, Tupel, Tupel, Tupel)
               deriving (Eq, Data, Typeable)
instance Version InviteKey
$(deriveSerialize ''InviteKey)
                        
instance Show InviteKey where
    show (InviteKey (a,b,c,d)) =
        let ts = [show t | t <- [a,b,c,d]]
        in intercalate "-" ts

--
-- RefKey
--
data RefKey = RefKey (Tupel, Tupel, Tupel, Tupel, Tupel)
               deriving (Eq, Data, Typeable)
instance Version RefKey
$(deriveSerialize ''RefKey)

instance Show RefKey where
    show (RefKey (a,b,c,d,e)) =
        let ts = [show t | t <- [a,b,c,d,e]]
        in intercalate "-" ts

  
--
-- Predicates
--
  
-- | 'validChar' is a predicate for the allowed characters in the 'RefKEy' and
--   in the 'InviteKey'.
--   allowed are all lower case ASCII characters and digits exept all 
--   vocals and the following: '0', '1', 'l'
validChar :: Char -> Bool
validChar c =
  (isAlphaNum c) && 
  (isDigit c || isAsciiLower c) &&
  (notElem c invalidChars) 
    where invalidChars = "aeiou01l"
  
validChars :: [Char]
validChars = [ c | c <- map chr [0..255], validChar c]

validTupel :: Int -> Tupel -> Bool
validTupel len (Tupel t) =
  (length t == len) &&
  (and $ map validChar t)

validKey len ts =
  (length ts == len) &&
  (and $ map (validTupel len) ts)
  
validInviteKey (InviteKey (a,b,c,d)) = 
  validKey inviteKeyTupelLen [a,b,c,d]

validRefKey (RefKey (a,b,c,d,e)) = 
  validKey refKeyTupelLen [a,b,c,d,e]

--
-- Parser
--
validCharParser = oneOf validChars
refTupelParser = count refKeyTupelLen validCharParser
inviteTupelParser = count inviteKeyTupelLen validCharParser

refKeyParser =
    do t1 <- refTupelParser
       optional $ char '-'
       t2 <- refTupelParser
       optional $ char '-'
       t3 <- refTupelParser
       optional $ char '-'
       t4 <- refTupelParser
       optional $ char '-'
       t5 <- refTupelParser
       return $ RefKey (Tupel t1, Tupel t2, Tupel t3, Tupel t4, Tupel t5)

inviteKeyParser =
    do t1 <- inviteTupelParser
       optional $ char '-'
       t2 <- inviteTupelParser
       optional $ char '-'
       t3 <- inviteTupelParser
       optional $ char '-'
       t4 <- inviteTupelParser
       return $ InviteKey (Tupel t1, Tupel t2, Tupel t3, Tupel t4)

--
-- Random Tupel and Key generation
--
randomChar = do  
  i <- getStdRandom (randomR (minBound, maxBound))
  return $ validChars !! (i `mod` length validChars)
         
randomTupel :: Int -> IO Tupel
randomTupel len = fmap Tupel $ randomTupel' len
randomTupel' len
  | (len < 0)  = error $ "len must not be negative: " ++ show len
  | (len == 0) = return []
  | otherwise  = do
    char <- randomChar
    chars <- randomTupel' (len - 1)
    return $ char : chars

randomInviteKey :: IO InviteKey
randomInviteKey = do
  a <- randomTupel inviteKeyTupelLen
  b <- randomTupel inviteKeyTupelLen
  c <- randomTupel inviteKeyTupelLen
  d <- randomTupel inviteKeyTupelLen
  return $ InviteKey (a,b,c,d)
  
randomRefKey :: IO RefKey
randomRefKey = do
  a <- randomTupel refKeyTupelLen
  b <- randomTupel refKeyTupelLen
  c <- randomTupel refKeyTupelLen
  d <- randomTupel refKeyTupelLen
  e <- randomTupel refKeyTupelLen  
  return $ RefKey (a,b,c,d,e)

instance Key RefKey where 
  randomKey = randomRefKey

instance Key InviteKey where
  randomKey = randomInviteKey
  
-- test function
mkKeyKeyPair :: Identity -> Integer -> IO (KeyPair RefKey InviteKey)
mkKeyKeyPair = mkKeyPair



