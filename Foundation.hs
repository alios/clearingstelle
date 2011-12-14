{-# LANGUAGE CPP, TemplateHaskell, QuasiQuotes, OverloadedStrings, MultiParamTypeClasses, TypeFamilies #-}

module Foundation
    ( CS (..)
    , CSRoute (..)
    , CSMessage (..)
    , resourcesCS
    , Handler
    , Widget
    , Form
    , maybeAuth
    , requireAuth
    , module Yesod
    , module Settings
    , module Model
    , StaticRoute (..)
    , AuthRoute (..)
    ) where

import Prelude
import Yesod hiding (Form)
import Yesod.Static (Static, base64md5, StaticRoute(..))
import Settings.StaticFiles
import Yesod.Auth
import Yesod.Auth.HashDB (getAuthIdHashDB, authHashDB)
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Logger (Logger, logLazyText)
import qualified Settings
import qualified Data.ByteString.Lazy as L
import qualified Database.Persist.Base
import Database.Persist.GenericSql
import Settings (widgetFile)
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
import Data.Text (Text)

#if PRODUCTION
import Network.Mail.Mime (sendmail)
#else
import qualified Data.Text.Lazy.Encoding
#endif

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data CS = CS
    { settings :: AppConfig DefaultEnv
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Base.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    }

-- Set up i18n messages. See the message folder.
mkMessage "CS" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype CSRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route CS = CSRoute
-- * Creates the value resourcesCS which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- CS. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the CSRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "CS" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm CS CS (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod CS where
    approot = appRoot . settings

    -- Place the session key file in the config folder
    encryptKey _ = fmap Just $ getKey "config/client_session_key.aes"

    defaultLayout widget = do
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            $(widgetFile "default-layout")
            $(widgetFile "clearingstelle")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    messageLogger y loc level msg =
      formatLogMessage loc level msg >>= logLazyText (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Enable Javascript async loading
    yepnopeJs _ = Just $ Right $ StaticR js_modernizr_js

-- How to run database actions.
instance YesodPersist CS where
    type YesodPersistBackend CS = SqlPersist
    runDB f = liftIOHandler
            $ fmap connPool getYesod >>= Database.Persist.Base.runPool (undefined :: Settings.PersistConfig) f


instance YesodAuth CS where
    type AuthId CS = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    
    -- Where to send a user after logout
    logoutDest _ = RootR
    getAuthId = getAuthIdHashDB AuthR (Just . UniqueUser)    
    authPlugins = [authHashDB (Just . UniqueUser)]

-- Sends off your mail. Requires sendmail in production!
deliver :: CS -> L.ByteString -> IO ()
#ifdef PRODUCTION
deliver _ = sendmail
#else
deliver y = logLazyText (getLogger y) . Data.Text.Lazy.Encoding.decodeUtf8
#endif

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage CS FormMessage where
    renderMessage _ _ = defaultFormMessage