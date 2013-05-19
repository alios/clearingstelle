{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Clearingstelle (getCheckoutR, postCheckoutR
                              ,getCreateKeysetR, postCreateKeysetR
                              ,getCleanupR, postCleanupR
			      ,getCompleteR
                              ,getDeactivateR, postDeactivateR
                              ,getMassrefkeysR, postMassrefkeysR) where

import Data.Text (Text)
import Data.List (permutations)
import Data.Maybe (isNothing, isJust, fromJust)
import qualified Data.Text as T (pack, unpack, concat, lines, unlines, breakOn)
import qualified Data.Text.Encoding as E
import Yesod
import Foundation
import Prelude
import System.Random (Random(..), randomIO)
import Model.Keys

checkoutRefKeyId :: Text
checkoutRefKeyId = "refKey"
checkoutInvKeyPrefix :: String
checkoutInvKeyPrefix = "invKey"
deactivateRefKeyField :: Text
deactivateRefKeyField = checkoutRefKeyId
massrefkeysField :: Text
massrefkeysField = checkoutRefKeyId

createKeyName, createKeyCount :: Text 
createKeyName = "createkeyname"
createKeyCount = "createkeycount"

cleanupInviteKeyField :: Text
cleanupInviteKeyField = "invitekeys"

cookieTimeout = 60

getCheckoutR :: Text -> Handler RepHtml
getCheckoutR dom = withDomainCheck dom $ \_ -> withCookieHandler dom $ defaultLayout $ do 
    setTitle "clearingstelle - Einladungs Schlüssel Checkout"
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
          let msg = T.concat [refKeyText, " ist kein gültiger Schlüssel. Bitte überprüfe deine Eingabe und propier es erneut."]
          $(logWarn) msg
          invalidArgs [msg]
        Just refKey -> do
          invKey' <- runDB $ checkoutKey domid refKey
          case (invKey') of
            Nothing -> do
              let msg = T.concat ["Der Eingegeben Key ", refKeyText, " konnte nicht aufgelöst werden. Entweder ist der Schlüssel nicht gültig, oder er wurde bereits verwendet."]         
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


getCompleteR :: Text -> Handler RepHtml
getCompleteR dom = withRole dom AdminRole $ \domid uid -> do
	n <- runDB $ completeKeysets 
	defaultLayout $ addHamlet [hamlet| <h2>created #{n} keypairs |]

getMassrefkeysR :: Text -> Handler RepHtml
getMassrefkeysR dom = withRole dom AdminRole $ \domid uid -> defaultLayout $ do
  setTitle "clearingstelle - mass RefKey lookup"
  $(widgetFile "massrefkeys")

postMassrefkeysR :: Text -> Handler RepHtml
postMassrefkeysR dom = withRole dom AdminRole $ \domid uid -> do
  refKeysField <- lookupPostParam massrefkeysField
  if (isNothing refKeysField)
    then do let msg = T.concat ["must supply post filed"]
            $(logWarn) msg
            invalidArgs $ [msg]
    else do
      let ls = T.lines $ fromJust refKeysField
      let ls2 = map (T.breakOn ",") ls 
      let refKeys' = (map (\(k,r) -> (parseKey k, r)) ls2) :: [(Maybe ReferenceKey, Text)]
      let refKeysOk = [(fromJust mk, r)  | (mk, r) <- refKeys', isJust mk]
      (invKeys, refKeysFail) <- runDB $ lks refKeysOk
      defaultLayout $ do
        let ikslines = T.unlines $ map (\(ik,t) -> T.concat [keyText ik, t]) invKeys
        let rkslines = T.unlines $ map keyText refKeysFail 
        addHamlet $ [hamlet| <h3>resolved invite keys:
                                <pre style="color:green;">#{ikslines}
                             <h3>unresolved ref keys (unused):
                                <pre style="color:red;">#{rkslines}
        |]
  where lks :: [(ReferenceKey, Text)] -> Database ([(InviteKey, Text)], [ReferenceKey])
        lks [] = return ([],[])
        lks ((rk, t):xs) = do
          ik <- lookupRefKey rk
          (iks, fks) <- lks xs
          case ik of        
            Nothing  -> return (iks, rk : fks)
            Just ik' -> return ((ik', t) : iks, fks)




getDeactivateR :: Text -> Handler RepHtml
getDeactivateR dom = withRole dom AdminRole $ \domid uuid -> defaultLayout $ do
  setTitle "clearingstelle - do deactivate"
  $(widgetFile "deactivate")


postDeactivateR :: Text -> Handler RepHtml
postDeactivateR dom = withRole dom AdminRole $ \domid uuid -> do
  refKeysField <- lookupPostParam deactivateRefKeyField
  if (isNothing refKeysField)
    then do let msg = T.concat ["must supply post field"]
            $(logWarn) msg
            invalidArgs $ [msg] 
    else do
      let ls = T.lines $ fromJust refKeysField
      let refKeys' = (map parseKey ls) :: [Maybe ReferenceKey]
      let refKeys = unJustList refKeys'
      let parseErrorRefKeys = unJustList $ zipWith (\a b -> if (isNothing a) then Just b else Nothing) refKeys' ls
      diks <- runDB $ deactivateKeys uuid refKeys
      let invalidRefKeys = unJustList $ zipWith (\a b -> if (isNothing a) then Just b else Nothing) diks ls
      let validKeys = unJustList diks
      let usedInviteKeys = map fst $ filter snd validKeys
      let unusedInviteKeys = map fst $ filter (not.snd) validKeys
      defaultLayout $ 
        do parseErrorRefKeysText <- fmap T.unlines $ liftIO $ shuffleList $ parseErrorRefKeys
           invalidRefKeysText <- fmap T.unlines $ liftIO $ shuffleList $ invalidRefKeys
           usedInviteKeysText <- fmap T.unlines $ liftIO $ shuffleList $ map keyText usedInviteKeys
           unusedInviteKeysText <- fmap T.unlines $ liftIO $ shuffleList $ map keyText unusedInviteKeys
             
           addHamlet $ [hamlet| <h3>Failed to parse:
                                <pre style="color:yellow;">#{parseErrorRefKeysText}
                                <h3>Unable to lookup:
                                <pre style="color:red;">#{invalidRefKeysText}
                                <h3>deactivated keys (unused):
                                <pre style="color:green;">#{unusedInviteKeysText}
                                <h3>deactivated keys (used):
                                <pre style="color:blue;">#{usedInviteKeysText}  |]
                
  

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
                             <pre style="color:orange;">#{iinvKeysText}
                             <h3>Failed to resolve:
                             <pre style="color:red;">#{irefKeysText}
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
    
