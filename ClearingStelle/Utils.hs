{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fglasgow-exts #-}

module ClearingStelle.Utils (trim, page, build_form) where

import Data.Char
import Data.Time
import Text.XHtml.Strict as XHTML

import Happstack.Data


trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace


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


instance Version Day
$(deriveSerialize ''Day)

instance Version UTCTime
instance Serialize UTCTime where
    getCopy =
        contain $ do day <- safeGet
                     dayTime <- fmap fromRational safeGet
                     return $ UTCTime day dayTime
    putCopy t = 
        contain $ do safePut $ utctDay t
                     safePut $ toRational $ utctDayTime t
    
