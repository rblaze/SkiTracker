{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Happstack.Lite
import System.Directory
import Text.Blaze.Html5 (Html, (!), form, input, toHtml)
import Text.Blaze.Html5.Attributes (action, enctype, name, type_, value)
import Text.Printf
import qualified Control.Exception as E 
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import HtmlMap
import PaintSki
import Parse
import SegmentedTrack

import Debug.Trace

main :: IO ()
main = serve Nothing myApp

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
    track = makeSegmentedTrack gpstrack
    runs = paintSkiTrack track
    html = makeMapPage runs

createTrackId :: BS.ByteString -> String
createTrackId track = parseTrack track `seq` concatMap (printf "%02x") (BS.unpack (MD5.hash track))

trackPage :: ServerPart Response
trackPage = msum [ uploadForm, saveTrack, showTrack ]
    where
    uploadForm :: ServerPart Response
    uploadForm = do
        method GET
        nullDir
        trace "Showing form" $ ok $ template "upload form" $
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

        let trackid = createTrackId xml

        join (liftIO $ E.catch (do
            renameFile tmpFile ("data/" ++ trackid)
            return (mkresp ("track/" ++ trackid))
           ) 
           (\(E.ErrorCall e) -> return (mkerr e)))
        where
        mkresp :: String -> ServerPart Response
        mkresp trackpath = trace "Saving track" $ seeOther trackpath $ 
                template "Redirecting..." $ do
                    H.toHtml ("Loading your track map" :: String)
                    H.br
                    H.toHtml ("Click " :: String)
                    H.a ! A.href (fromString trackpath) $ H.toHtml ("here" :: String)
                    H.toHtml (" if automatic redirection fails" :: String)
        mkerr :: String -> ServerPart Response
        mkerr msg = ok $ template "Parse error" $ H.toHtml (printf "%s" msg :: String)

    showTrack :: ServerPart Response
    showTrack = do
        method GET
        path $ \trackid -> do
            xml <- liftIO $ BS.readFile ("data/" ++ trackid)
            trace "Processing track" $ ok $ toResponse $ makeMap xml
