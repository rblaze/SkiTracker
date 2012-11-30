{-# LANGUAGE OverloadedStrings #-}

module HtmlMap(makeMapPage) where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.List
import Text.Blaze ((!))
import Text.Printf

import Geo
import PaintSki
import SegmentedTrack

googleApiKey :: String
googleApiKey = "AIzaSyCtiE9l_Rhk7LNF-ImN5TJlkKTZWfL46XM"

centerPoint :: [SkiRun] -> Position
centerPoint track = Position (avg maxx minx) (avg maxy miny)
    where
    avg a b = (a + b) / 2
    minMax (pxmin, pymin, pxmax, pymax) (xmin, ymin, xmax, ymax)
        = (min xmin pxmin, min ymin pymin, max xmax pxmax, max ymax pymax)
    segmentLimits TrackSegment{tsStartPos = Position sx sy, tsEndPos = Position ex ey}
        = (min sx ex, min sy ey, max sx ex, max sy ey)
    (minx, miny, maxx, maxy) = foldr1 minMax $ map segmentLimits $ concatMap runPoints track

printPosition :: Position -> String
printPosition (Position x y) = printf "new google.maps.LatLng(%f, %f)" (x / pi * 180) (y / pi * 180)

printSegment :: String -> [TrackSegment] -> String
printSegment name track = "    var " ++ name ++ " = [\n        "
        ++ intercalate ",\n        " (map (printPosition . tsStartPos) track)
        ++ ",\n        "
        ++ printPosition (tsEndPos $ last track) ++ "\n"
        ++ "     ];\n"

addSegmentToMap :: Int -> SkiRun -> String
addSegmentToMap uniq track = printSegment pathname (runPoints track)
        ++ polyline linename pathname
        ++ "    " ++ linename ++ ".setMap(map);\n"
        ++ "    google.maps.event.addListener(" ++ linename ++ ", 'click', function(event) { onPathClick(event, \"" ++ message ++  "\");});\n"
    where
    message = show (runType track) ++ " " ++ show (runStartTime track) ++ "<br>Duration: " ++ show (runDuration track)
    color = case runType track of
        Idle -> "#00FF00"
        Track -> "#FF0000"
        Lift -> "#0000FF"
    pathname = printf "path%dpoints" uniq
    linename = printf "path%d" uniq
    polyline ln pn = "    var " ++ ln ++ " = new google.maps.Polyline({\n\
\        path: " ++ pn ++ ",\n\
\        strokeColor: \"" ++ color ++ "\",\n\
\        strokeWeight: 2\n\
\      });\n"

generateScript :: [SkiRun] -> H.Html
generateScript track = do
    H.script ! A.type_ "text/javascript" ! A.src (H.toValue $ "http://maps.googleapis.com/maps/api/js?key=" ++ googleApiKey ++ "&sensor=false") $ ""
    H.script ! A.type_ "text/javascript" $ H.toHtml script
    where
    script :: String
    script = header ++ path ++ footer

    firstPoint = head $ runPoints $ head track

    header = "/****** begin script header *******/\n\
\function initialize() {  \n\
\    var centerPoint = " ++ printPosition (centerPoint track) ++ ";  \n\
\//    var trackStart = " ++ printPosition (tsStartPos firstPoint) ++ ";\n\
\    var myOptions = {  \n\
\      zoom: 14,  \n\
\      center: centerPoint,  \n\
\      mapTypeId: google.maps.MapTypeId.TERRAIN  \n\
\    };  \n\
\    var map = new google.maps.Map(document.getElementById(\"map_canvas\"), myOptions);\n\
\    var info = new google.maps.InfoWindow({ content: '" ++ "FIXME" ++ "' });  \n\
\    function onPathClick(event, message) {   \n\
\        info.setContent(message);  \n\
\        info.open(map);   \n\
\        info.setPosition(event.latLng);  \n\
\    };  \n\
\//    info.open(map);\n\
\//    info.setPosition(trackStart);\n\
\/******* end script header *******/\n"

    footer = "}"
    path = concat $ zipWith addSegmentToMap [0..] track

makeMapPage :: [SkiRun] -> H.Html
makeMapPage track = H.docTypeHtml $ do
    H.head $ do
        H.title "Track map"
        H.meta ! A.name "viewport" ! A.content "initial-scale=1.0, user-scalable=no"
        H.style ! A.type_ "text/css" $ "html { height: 100% } body { height: 100%; margin: 0; padding: 0 } #map_canvas { height: 100% }"
        generateScript track
    H.body ! A.onload "initialize()" $ H.div ! A.id "map_canvas" $ ""
