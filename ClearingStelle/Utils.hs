{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fglasgow-exts #-}

module ClearingStelle.Utils (trim, shuffle, page, build_form) where

import Data.Char
import Data.Time
import Text.XHtml.Strict as XHTML
import System.Random 

import Happstack.Data


trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

shuffle :: [a] -> IO [a]
shuffle = shuffle' []
    where shuffle' l [] = do return l
          shuffle' d s = do
            i <- getStdRandom (randomR (0, length s - 1))
            let (as, b:bs) = splitAt i s
            shuffle' (b:d) (as ++ bs)

mmeta e c = meta ! [ httpequiv e, content c ]

page :: String -> Html -> Html
page t c = 
    let headers = [ thetitle << t
                  , mmeta "expires" "0"
                  , mmeta "cache-control" "no-cache"
                  , mmeta "pragma" "no-cache"
                  , thelink noHtml ! [rel "stylesheet"
                                     ,href "/static/clearing.css"
                                     ,thetype "text/html" ]
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
    in ((h2 << lbl) +++ br +++ form' << ((foldl (+++) noHtml fss') +++ (submit "Done" "Done")))



    
