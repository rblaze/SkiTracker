{-# LANGUAGE OverloadedStrings #-}

module AppServer(appServerMain) where

import Control.Monad.IO.Class
import Data.Function (on)
import Data.List (groupBy)
import Happstack.Lite
import System.Directory
import Text.Blaze.Html5 (Html, (!), form, input, toHtml)
import Text.Blaze.Html5.Attributes (action, enctype, name, type_, value)
import Text.Printf
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

--import Happstack.Server.SURI

import Maps
import Markup
import Parse

import Debug.Trace

appServerMain :: IO ()
appServerMain = serve Nothing myApp

myApp :: ServerPart Response
myApp = msum [
    dir "data" mzero,
    dir "static" serveStatic,
    dir "track" trackPage
    ]

serveStatic :: ServerPart Response
serveStatic = serveDirectory DisableBrowsing [] "static"

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

createTrackId :: BS.ByteString -> String
createTrackId track = parseTrack track `seq` concatMap (printf "%02x") (BS.unpack (MD5.hash track))

trackPage :: ServerPart Response
trackPage = msum [ uploadForm, saveTrack, showTrack ]
    where
    uploadForm :: ServerPart Response
    uploadForm = do
        method GET
        nullDir
        ok $ template "upload form" $
            form ! action "/track" ! enctype "multipart/form-data" ! A.method "POST" $ do
                input ! type_ "file" ! name "track"
                H.br
                input ! type_ "submit" ! value "upload"
    
    saveTrack :: ServerPart Response
    saveTrack = do
        method POST
        nullDir
        
        (tmpFile, _, _) <- lookFile "track"
        xml <- liftIO $ BS.readFile tmpFile
        let trackid = trace tmpFile $ createTrackId xml

        liftIO $ renameFile tmpFile ("data/" ++ trackid)
        trace "Saving track" $ seeOther ("track/" ++ trackid)  $ toResponse $ H.preEscapedString "redirecting to your track map"

    showTrack :: ServerPart Response
    showTrack = do
        method GET
        path $ \trackid -> do
            xml <- liftIO $ BS.readFile ("data/" ++ trackid)
            trace "Processing track" $ ok $ toResponse $ makeMap xml