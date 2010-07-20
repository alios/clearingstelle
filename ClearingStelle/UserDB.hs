{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fglasgow-exts -XNoMonomorphismRestriction #-}

module ClearingStelle.UserDB
    (adminRole, managerRole, inviteSiteRole, requestSiteRole
    ,UserDB, roleAuth
    ,admin_adduser_get, admin_adduser_post) where

import qualified Data.Map as M
import Data.Generics(Data)
import Data.Typeable
import Control.Monad.State 
import Control.Monad.Reader

import Text.XHtml.Strict as XHTML

import Happstack.Data
import Happstack.State
import Happstack.Server.SimpleHTTP



page :: String -> Html -> Html
page t c = 
    let headers = [thetitle << t
                  ]
        hdr = header << foldl (+++) noHtml headers
        bdy = body << ((h1 << t) +++ c)
    in hdr +++ bdy 
       


build_form :: String -> URL -> [(String, [(String, Html)])] -> Html
build_form lbl action fss =
    let fss' = [ fieldset << ((legend $ toHtml l) +++ build_fieldset fs)  | (l,fs) <- fss ] 
        build_fieldset fs = 
            foldl (+++) noHtml 
                      [((label $ toHtml n) ! [thefor n]) +++ (e ! [name n, identifier n] +++ br)| (n,e) <- fs]
        form' = form ! [XHTML.method "post", XHTML.action action]
    in ((h2 << lbl) +++ br +++ form' << ((foldl (+++) noHtml fss') +++ (submit "Save" "Save")))


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


admin_adduser_get = 
    ok $ toResponse $ admin_adduser_page


admin_adduser_post = undefined



data Role = Admin | Manager | InviteSite | RequestSite
          deriving (Show, Read, Eq, Enum, Data, Typeable)
instance Version Role
$(deriveSerialize ''Role)

adminRole = Admin
managerRole = Manager
inviteSiteRole = InviteSite
requestSiteRole = RequestSite




data User = User {
      user_email :: String,
      user_pw :: String,
      user_roles :: [ Role ]
    } deriving (Show, Read, Eq, Data, Typeable)
instance Version User
$(deriveSerialize ''User)


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


addUser :: String -> String -> [ Role ] -> Update UserDB ()
addUser u p rs = modify $ (\db -> UserDB $ add_user u p rs $ userdb_users db)
    where add_user u p rs users 
              | null u = fail "username must not be empty"
              | null p = fail "password must not be empty"
              | elem u $ [user_email u | u <- users] = fail $ "user " ++ u ++ " already exists"
              | otherwise = (User u p rs) : users
    

$(mkMethods ''UserDB ['getUserMap, 'addUser])

roleAuth :: Role -> ServerPart Response -> ServerPart Response
roleAuth r p =
    do usermap <- query $ GetUserMap r
       basicAuth "please authorize yourself" usermap p



