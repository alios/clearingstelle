{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Clearingstelle (getCheckoutR, postCheckoutR) where

import Data.Text (Text)
import qualified Data.Text as T (pack, unpack, concat)
import qualified Data.Text.Encoding as E
import Yesod
import Foundation

import Model.Keys

checkoutRefKeyId :: Text
checkoutRefKeyId = "refKey"
checkoutInvKeyPrefix :: String
checkoutInvKeyPrefix = "invKey"

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
      Nothing -> invalidArgs $ [T.concat ["must supply a post field ", checkoutRefKeyId]]
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
              setCookie cookieTimeout (cookieNameAscii dom) (E.encodeUtf8 invKeyText)
              checkoutWidget invKeyText
