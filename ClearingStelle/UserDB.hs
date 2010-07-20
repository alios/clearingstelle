{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fglasgow-exts -XNoMonomorphismRestriction #-}

module ClearingStelle.UserDB
    (Role(..), UserDB, roleAuth
    ,admin_adduser_get, admin_adduser_post) where

import Data.Maybe
import qualified Data.Map as M
import Data.Generics(Data)
import Data.Typeable
import Control.Monad.State 
import Control.Monad.Reader

import Text.XHtml.Strict as XHTML

import Happstack.Data
import Happstack.State
import Happstack.Server.SimpleHTTP

import System.Log.Logger

import ClearingStelle.Utils

admin_adduser_page :: Html
admin_adduser_page =
    let f = build_form "Add User" "/admin/adduser" 
            [("User Data", [("Email", textfield "") ,("Password", password "")])
            ,("Roles", [("Admin", checkbox "isAdmin" "isAdmin")
                       ,("Manager", checkbox "isManager" "isManager")
                       ,("InviteSite", checkbox "isInviteSite" "isInviteSite")
                       ,("RequestSite", checkbox "isRequestSite" "isRequestSite")
                       ])
            ]
    in page "Add User" f

data Role = Admin | Manager | InviteSite | RequestSite
          deriving (Show, Read, Eq, Enum, Data, Typeable)
instance Version Role
$(deriveSerialize ''Role)

data User = User {
      user_email :: String,
      user_pw :: String,
      user_roles :: [ Role ]
    } deriving (Show, Read, Eq, Data, Typeable)
instance Version User
$(deriveSerialize ''User)

instance FromData User where
    fromData = do
      ps <- fmap M.fromList lookPairs
      email         <- fmap trim $ look "Email"
      password      <- fmap trim $ look "Password"
      let roles' = [(Admin, inRole "isAdmin")
                   ,(Manager, inRole "isManager")
                   ,(InviteSite, inRole "isInviteSite")
                   ,(RequestSite, inRole "isRequestSite")]
          roles  = [r | (r, e) <- roles', e]
          inRole s = s `M.member` ps
      return $ User email password roles


newtype UserDB = UserDB {
      userdb_users :: [ User ]
    } deriving (Show, Read, Eq, Data, Typeable)

instance Version UserDB
$(deriveSerialize ''UserDB)

instance Component UserDB where
    type Dependencies UserDB = End
    initialValue = UserDB [ User "alios" "test" [Admin]]



getUserMap :: Role -> Query UserDB (M.Map String String)
getUserMap r =
    do userdb <- fmap userdb_users ask
       let us = filter (\u -> r `elem` (user_roles u)) userdb
       return $ M.fromList [ (user_email u, user_pw u) | u <- us ]


addUser :: User -> Update UserDB ()
addUser u 
    | null $ user_email u = fail "username must not be empty"
    | null $ user_pw u = fail "password must not be empty"
    | otherwise = 
        do us <- fmap userdb_users getState
           if (elem (user_email u) $ [user_email u | u <- us]) then 
               fail $ "user " ++ (user_email u) ++ " already exists" else
               putState $ UserDB (u:us)

$(mkMethods ''UserDB ['getUserMap, 'addUser])


admin_adduser_get = 
    ok $ toResponse $ admin_adduser_page


admin_adduser_post = do
  u <- getData
  if (isJust u) then 
      do let u' = fromJust u
         update $ AddUser u'
         ok $ toResponse $ page "User created" << h2 << ("user " ++ user_email u' ++ " created.") else 
      badRequest $ toResponse $  "invalid/insufficient POST data" 

roleAuth :: Role -> ServerPart Response -> ServerPart Response
roleAuth r p =
    do usermap <- query $ GetUserMap r
       basicAuth "please authorize yourself" usermap p



