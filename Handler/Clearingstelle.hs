{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Clearingstelle (getCheckoutR, postCheckoutR
                              ,getCreateKeysetR, postCreateKeysetR
                              ,getCleanupR, postCleanupR) where

import Data.Text (Text)
import Data.List (permutations)
import Data.Maybe (isNothing, isJust, fromJust)
import qualified Data.Text as T (pack, unpack, concat, lines, unlines)
import qualified Data.Text.Encoding as E
import Yesod
import Foundation
import System.Random (Random(..), randomIO)
import Model.Keys

checkoutRefKeyId :: Text
checkoutRefKeyId = "refKey"
checkoutInvKeyPrefix :: String
checkoutInvKeyPrefix = "invKey"

createKeyName, createKeyCount :: Text 
createKeyName = "createkeyname"
createKeyCount = "createkeycount"

cleanupInviteKeyField :: Text
cleanupInviteKeyField = "invitekeys"

cookieTimeout = 60

getCheckoutR :: Text -> Handler RepHtml
getCheckoutR dom = withDomainCheck dom $ \_ -> withCookieHandler dom $ defaultLayout $ do 
    setTitle "clearingstelle - do checkout"
    $(widgetFile "checkout-form")
      
postCheckoutR :: Text -> Handler RepHtml
postCheckoutR dom = withDomainCheck dom $ \domid -> withCookieHandler dom $ do
    postField <- lookupPostParam checkoutRefKeyId
    case (postField) of
      Nothing -> do
        let msg = T.concat ["must supply a post field ", checkoutRefKeyId]
        $(logWarn) msg
        invalidArgs $ [msg]
      Just refKeyText -> case (parseKey refKeyText :: Maybe ReferenceKey) of
        Nothing -> do 
          let msg = T.concat [refKeyText, " is not a valid refkey."]
          $(logWarn) msg
          invalidArgs [msg]
        Just refKey -> do
          invKey' <- runDB $ checkoutKey domid refKey
          case (invKey') of
            Nothing -> do
              let msg = T.concat ["unable to check out key ", refKeyText]         
              $(logWarn) msg
              invalidArgs [msg]
            Just invKey -> do
              let invKeyText = keyText invKey
              $(logDebug) $ T.concat ["checked out refkey ", refKeyText ]
              setCookie cookieTimeout (cookieNameAscii dom) (E.encodeUtf8 invKeyText)
              checkoutWidget invKeyText

getCreateKeysetR :: Text -> Handler RepHtml
getCreateKeysetR dom = withRole dom AdminRole $ \domid uuid -> defaultLayout $ do
  setTitle "clearingstelle - create keyset"
  $(widgetFile "createkeyset")
  
postCreateKeysetR :: Text -> Handler RepHtml
postCreateKeysetR dom = withRole dom AdminRole $ \domid uid -> do
  nameF <- lookupPostParam createKeyName
  countF <- lookupPostParam createKeyCount
  if ((isNothing nameF) || (isNothing countF))
    then do let msg = T.concat ["must supply all post fields"]
            $(logWarn) msg
            invalidArgs $ [msg]
    else do let nameF' = fromJust nameF
            let countF' = (read . T.unpack . fromJust) countF
            _ <- runDB $ insertKeyset domid nameF' countF' uid
            $(logDebug) $ T.concat ["created new keyset '", nameF', "' key count: "
                                   , (T.pack . show)countF', " for keys" ]
            defaultLayout $ addHamlet [hamlet| <h2>triggered creation of new keyset |]

getCleanupR :: Text -> Handler RepHtml
getCleanupR dom = withRole dom AdminRole $ \domid uuid -> defaultLayout $ do
  setTitle "clearingstelle - do cleanup"
  $(widgetFile "cleanup")
  
postCleanupR :: Text -> Handler RepHtml
postCleanupR dom = withRole dom AdminRole $ \domid uuid -> do
  invKeys <- lookupPostParam cleanupInviteKeyField
  if (isNothing invKeys)
    then do let msg = T.concat ["must supply post field"]
            $(logWarn) msg
            invalidArgs $ [msg] 
    else do
      let ls = T.lines $ fromJust invKeys
      let invKeys = (map parseKey ls) :: [Maybe InviteKey]
      let vinvKeys = unJustList invKeys
      let iinvKeys = unJustList $ zipWith (\a b -> if (isNothing a) then Just b else Nothing) invKeys ls
      refKeys <- runDB $ cleanupKeys domid vinvKeys
      let vrefKeys = unJustList refKeys
      let irefKeys = unJustList $ zipWith (\a b -> if (isNothing a) then Just b else Nothing) refKeys vinvKeys
      defaultLayout $ do
        iinvKeysText <- fmap T.unlines $ liftIO $ shuffleList iinvKeys 
        irefKeysText <- fmap T.unlines $ liftIO $ shuffleList $ map keyText irefKeys
        refKeysText  <- fmap T.unlines $ liftIO $ shuffleList $ map keyText vrefKeys
            
        addHamlet $ [hamlet| <h3>Failed to parse:
                             <pre>#{iinvKeysText}
                             <h3>Failed to resolve:
                             <pre>#{irefKeysText}
                             <h3>Reference Keys:
                             <pre>#{refKeysText}  |]
      
      
      
      
unJustList l = map fromJust $ filter isJust l
      
      
shuffleList :: [a] -> IO [a]
shuffleList as = do
  ls <- shuffleList' (as, [])
  return $ snd ls
  
shuffleList' :: ([a],[a]) -> IO ([a],[a])
shuffleList' ([], bs) = return ([], bs) 
shuffleList' (as, bs) = do
  r <- randomIO
  let i = r `mod` (length as)
  let res = splitAt i as
  let arg = case (res) of    
        ([], (x:xs)) -> (xs, x : bs)
        ((x:xs), []) -> (xs, x : bs)
        (xs, y:ys) -> (ys ++ xs, y : bs)
  shuffleList' arg
      
      
      
checkoutWidget invKey = defaultLayout $ do
      setTitle "clearingstelle - checkout"
      $(widgetFile "checkout")



domainId :: Text -> Handler (Maybe DomainId)
domainId dom = runDB $ selectDomain dom

withDomainCheck :: Text -> (DomainId -> Handler a) -> Handler a
withDomainCheck dom h = do 
  domid' <- domainId dom
  case (domid') of
    Nothing -> do 
      $(logWarn) $ T.concat ["domain ", dom, " not found"]
      notFound
    Just domid -> h domid

withRole :: Text -> RoleType -> (DomainId -> UserId -> Handler a) -> Handler a
withRole dom role h = withDomainCheck dom $ \domid -> do
  (uid, u) <- requireAuth
  inRole <- runDB $ userInRole uid role domid
  if (not inRole)
    then do let msg = T.concat [userIdent u, " is not allowed to call getCreateKeySet"]
            $(logWarn) msg
            permissionDenied msg
    else h domid uid
  
cookieName dom = T.pack $ checkoutInvKeyPrefix ++ "_" ++ (T.unpack dom)
cookieNameAscii dom = E.encodeUtf8 $ cookieName dom

withCookieHandler dom h = do
  c <- lookupCookie $ cookieName dom
  case c of
    Just invKey -> checkoutWidget invKey
    Nothing -> h
    
