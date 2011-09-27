{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Clearingstelle (getCheckoutR, postCheckoutR
                              ,getCreateKeysetR, postCreateKeysetR) where

import Data.Text (Text)
import Data.Maybe (isNothing, fromJust)
import qualified Data.Text as T (pack, unpack, concat)
import qualified Data.Text.Encoding as E
import Yesod
import Foundation

import Model.Keys

checkoutRefKeyId :: Text
checkoutRefKeyId = "refKey"
checkoutInvKeyPrefix :: String
checkoutInvKeyPrefix = "invKey"

createKeyName :: Text 
createKeyCount :: Text
createKeyName = "createkeyname"
createKeyCount = "createkeycount"

cookieTimeout = 60

cookieName dom = T.pack $ checkoutInvKeyPrefix ++ "_" ++ (T.unpack dom)
cookieNameAscii dom = E.encodeUtf8 $ cookieName dom

withCookieHandler dom h = do
  c <- lookupCookie $ cookieName dom
  case c of
    Just invKey -> checkoutWidget invKey
    Nothing -> h
    

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
  

checkoutWidget invKey = defaultLayout $ do
      setTitle "clearingstelle - checkout"
      $(widgetFile "checkout")

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

