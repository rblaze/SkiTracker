{-# LANGUAGE OverloadedStrings #-}

module AppServer(appServerMain) where

import Control.Monad.IO.Class
import Data.Function (on)
import Data.List (groupBy)
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), form, input, toHtml)
import Text.Blaze.Html5.Attributes (action, enctype, name, type_, value)
import qualified Data.ByteString.Lazy as BS
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Maps
import Markup
import Parse

import Debug.Trace

appServerMain :: IO ()
appServerMain = serve Nothing myApp

myApp :: ServerPart Response
myApp = msum [
    dir "static" static,
    dir "track" trackPage
    ]

static :: ServerPart Response
static = serveDirectory DisableBrowsing [] "static"

template :: String -> Html -> Response
template title body = toResponse $
    H.html $ do
        H.head $
            H.title (toHtml title)
        H.body 
            body

makeMap :: BS.ByteString -> H.Html
makeMap xml = html
    where
    gpstrack = parseTrack xml
    track = makeTrackInfo gpstrack
    marked = markLifts track
    parts = groupBy ((==) `on` siType) marked
    html = getMapPage parts
    

trackPage :: ServerPart Response
trackPage = msum [ uploadForm, processTrack ]
    where
    uploadForm :: ServerPart Response
    uploadForm = do
        method GET
        ok $ template "upload form" $
            form ! action "/track" ! enctype "multipart/form-data" ! A.method "POST" $ do
                input ! type_ "file" ! name "track"
                H.br
                input ! type_ "submit" ! value "upload"
    
    processTrack :: ServerPart Response
    processTrack = trace "Processing request" $ do
        (tmpFile, _, _) <- lookFile "track"
        xml <- liftIO $ BS.readFile tmpFile
        ok $ toResponse $ makeMap xml
