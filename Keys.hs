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

{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, TemplateHaskell #-}

module Keys ( RefKey, InviteKey
            , randomRefKey, randomInviteKey
            , validInviteKey, validRefKey) where

import System.Random
import Data.Typeable
import Data.Data
import Data.Char        
import Data.List
import Text.ParserCombinators.ReadP
import Database.Persist (PersistField(..))
import Database.Persist.TH

refKeyTupelLen :: Int
refKeyTupelLen = 4
inviteKeyTupelLen :: Int
inviteKeyTupelLen = 5

newtype Tupel = Tupel String
                deriving (Eq, Data, Typeable)

instance Show Tupel where
  show (Tupel s) = s


newtype RefKey = RefKey (Tupel, Tupel, Tupel, Tupel, Tupel)
               deriving (Eq, Data, Typeable)

newtype InviteKey = InviteKey (Tupel, Tupel, Tupel, Tupel)
               deriving (Eq, Data, Typeable)
                        
derivePersistField "RefKey"
derivePersistField "InviteKey"

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

validKey :: Int -> [Tupel] -> Bool
validKey len ts =
  (length ts == len) &&
  (and $ map (validTupel len) ts)
  
validInviteKey :: InviteKey -> Bool
validInviteKey (InviteKey (a,b,c,d)) = 
  validKey inviteKeyTupelLen [a,b,c,d]

validRefKey :: RefKey -> Bool
validRefKey (RefKey (a,b,c,d,e)) = 
  validKey refKeyTupelLen [a,b,c,d,e]


--
-- Parser
--
validCharParser :: ReadP Char
validCharParser = satisfy validChar
refTupelParser :: ReadP [Char]
refTupelParser = count refKeyTupelLen validCharParser
inviteTupelParser :: ReadP [Char]
inviteTupelParser = count inviteKeyTupelLen validCharParser

refKeyParser :: ReadP RefKey
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

inviteKeyParser :: ReadP InviteKey
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
-- InviteKey
--

instance Show InviteKey where
    show (InviteKey (a,b,c,d)) =
        let ts = [show t | t <- [a,b,c,d]]
        in intercalate "-" ts

instance Read InviteKey where
  readsPrec _ = readP_to_S inviteKeyParser


instance Show RefKey where
    show (RefKey (a,b,c,d,e)) =
        let ts = [show t | t <- [a,b,c,d,e]]
        in intercalate "-" ts

instance Read RefKey where
  readsPrec _ = readP_to_S refKeyParser

--
-- Random Tupel and Key generation
--
randomChar :: IO Char
randomChar = do  
  i <- getStdRandom (randomR (minBound, maxBound))
  return $ validChars !! (i `mod` length validChars)
         
randomTupel :: Int -> IO Tupel
randomTupel len = fmap Tupel $ randomTupel' len
  where randomTupel' l
          | (l < 0)  = error $ "len must not be negative: " ++ show len
          | (l == 0) = return []
          | otherwise  = do
              c <- randomChar
              cs <- randomTupel' (len - 1)
              return $ c : cs

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

