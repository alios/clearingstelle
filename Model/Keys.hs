{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, EmptyDataDecls #-}

module Model.Keys (KeyC(..), LQFBInviteKey(..), LQFBReferenceKey(..)) where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

import Data.Text (Text)
import Data.List
import Data.List.Split (splitEvery)
import qualified Data.Text as T (pack, unpack, length)
import System.Random (Random(..), randomIO)

class KeyC k where
  data KeyT k :: *
  parseKey :: Text -> Maybe (KeyT k)
  keyText :: KeyT k -> Text
  keyString :: KeyT k -> String
  randomKey :: IO (KeyT k)

lqfbKeyLength = 20
lqfbRefKeyTupelLen = 4
lqfbInvKeyTupelLen = 5
lqfbTupelSeperator = '-'
lqfbRefKeyTupels = lqfbKeyLength `div` lqfbRefKeyTupelLen
lqfbRefKeyTextLen = lqfbRefKeyTupels + lqfbKeyLength - 1
lqfbInvKeyTupels = lqfbKeyLength `div` lqfbInvKeyTupelLen
lqfbInvKeyTextLen = lqfbInvKeyTupels + lqfbKeyLength - 1


validLQFBKeyChars :: [Char]
validLQFBKeyChars =
  filter (\c -> notElem c "aeiou01l") $ ['a'..'z'] ++ ['0' .. '9']
  
takeLQFBKeyChars :: Int -> Char
takeLQFBKeyChars i = validLQFBKeyChars !! (i `mod` (length validLQFBKeyChars))
  

lqfbTupelSplit n = concat . intersperse [lqfbTupelSeperator] . splitEvery n
lqfbTupelRandom n  = sequence $ replicate lqfbKeyLength $ fmap n randomIO

lqfbParser tl tc = 
  let tupelParser = count tl $ oneOf validLQFBKeyChars
  in do
    spaces
    xs <- count (tc - 1) $ do 
      t <- tupelParser
      _ <- char lqfbTupelSeperator
      return t
    x <- tupelParser
    return $ xs ++ [x]


data LQFBInviteKey 
instance KeyC LQFBInviteKey where
  data KeyT LQFBInviteKey = LQFBInviteKey [Char]
                            deriving (Eq, Show, Read)
  keyText (LQFBInviteKey s) = T.pack $ lqfbTupelSplit lqfbInvKeyTupelLen s
  keyString (LQFBInviteKey s) = s
  randomKey = fmap LQFBInviteKey $ lqfbTupelRandom takeLQFBKeyChars
  parseKey t =
    let p = lqfbParser lqfbInvKeyTupelLen lqfbInvKeyTupels
        t' = T.unpack t
    in case (runParser p () t' t') of
      Left _ -> Nothing
      Right ts -> Just $ LQFBInviteKey $ concat ts


data LQFBReferenceKey 

instance KeyC LQFBReferenceKey where
  data KeyT LQFBReferenceKey = LQFBReferenceKey [Char]
                            deriving (Eq, Show, Read)
  keyText (LQFBReferenceKey s) = T.pack $ lqfbTupelSplit lqfbRefKeyTupelLen s
  keyString (LQFBReferenceKey s) = s
  randomKey = fmap LQFBReferenceKey $ lqfbTupelRandom takeLQFBKeyChars
  parseKey t =
    let p = lqfbParser lqfbRefKeyTupelLen lqfbRefKeyTupels
        t' = T.unpack t
    in case (runParser p () t' t') of
      Left _ -> Nothing
      Right ts -> Just $ LQFBReferenceKey $ concat ts

