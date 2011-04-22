{-# LANGUAGE TypeFamilies, 
             QuasiQuotes, 
             TemplateHaskell,
             OverloadedStrings,
             MultiParamTypeClasses #-}

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

module Clearingstelle (CS (..)) where

import Yesod
import Yesod.Helpers.Static
import Yesod.Request
import qualified Data.Text as T
import Control.Applicative 

import Settings 

data CS = CS { csStatic :: Static }
type Handler = GHandler CS CS

mkYesod "CS" [parseRoutes|
  / ClearingR GET
  /checkout CheckoutR POST
  /impressum ImpressumR GET
  /static StaticR Static csStatic
|]

getImpressumR = defaultLayout $ addHamlet $(hamletFile "impressum")
getClearingR = defaultLayout $ addHamlet $(hamletFile "clearing")

postCheckoutR = do
  refKey' <- lookupPostParam refKeyParam
  case (refKey') of 
    Nothing ->  do
      let errMsg = T.concat [(T.pack "checkout called without field "), refKeyParam]
      $(logInfo) errMsg
      invalidArgs [errMsg]    
    Just refKey -> do
      case (validateKey refKey) of
        Nothing -> do
          let errMsg = T.concat [(T.pack "key '"), refKey, (T.pack "' ist not valid")] 
          $(logInfo) errMsg
          invalidArgs [errMsg]
        Just invKey -> do
          $(logInfo) (T.concat [(T.pack "check out of refKey "), refKey])
          defaultLayout $ addHamlet $(hamletFile "invitekey")


validateKey :: T.Text -> Maybe T.Text
validateKey k = Just $ T.pack "FOOOOOOBAAAAR"

instance Yesod CS where 
  approot _ = "http://localhost:3000"
  defaultLayout widget = do
    pc <- widgetToPageContent $ do
      addCassius $(cassiusFile "default-layout")
      widget
    hamletToRepHtml $(hamletFile "default-layout")

