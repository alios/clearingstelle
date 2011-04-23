{-# LANGUAGE TemplateHaskell, 
             StandaloneDeriving, 
             DeriveDataTypeable, 
             Generics,
             FlexibleContexts #-}

module Keys where

import System.Random
import Data.Typeable
import Data.Data
import Data.Char        
import Data.Maybe
import Data.List
import Text.ParserCombinators.ReadP
import qualified Database.Persist.Base as P
import qualified Data.Text as T

refKeyTupelLen = 4
inviteKeyTupelLen = 5

--
-- Tupel
--
newtype Tupel = Tupel String
                deriving (Eq, Data, Typeable)

instance Show Tupel where
  show (Tupel s) = s



--
-- InviteKey
--
data InviteKey = InviteKey (Tupel, Tupel, Tupel, Tupel)
               deriving (Eq, Data, Typeable)
                        
instance Show InviteKey where
    show (InviteKey (a,b,c,d)) =
        let ts = [show t | t <- [a,b,c,d]]
        in intercalate "-" ts

instance Read InviteKey where
  readsPrec _ = readP_to_S inviteKeyParser

instance P.PersistField InviteKey where
  toPersistValue = P.PersistText . T.pack . show
  fromPersistValue (P.PersistText t) = read $ T.unpack t 
  sqlType (InviteKey _) = P.SqlString

--
-- RefKey
--
data RefKey = RefKey (Tupel, Tupel, Tupel, Tupel, Tupel)
               deriving (Eq, Data, Typeable)

instance Show RefKey where
    show (RefKey (a,b,c,d,e)) =
        let ts = [show t | t <- [a,b,c,d,e]]
        in intercalate "-" ts

instance Read RefKey where
  readsPrec _ = readP_to_S refKeyParser

instance P.PersistField RefKey where
  toPersistValue = P.PersistText . T.pack . show
  fromPersistValue (P.PersistText t) = read $ T.unpack t 
  sqlType (RefKey _) = P.SqlString


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

validCharParser = satisfy validChar
refTupelParser = count refKeyTupelLen validCharParser
inviteTupelParser = count inviteKeyTupelLen validCharParser

refKeyParser = do
  t1 <- refTupelParser
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

