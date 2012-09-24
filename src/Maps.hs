{-# LANGUAGE OverloadedStrings #-}

module Maps(getMapPage) where

import Control.Monad.State
import Data.List (intercalate)
import Data.Time.Format(formatTime)
import System.Locale (defaultTimeLocale)
import Text.Printf

import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Markup
import Track
import Util

getMapScript :: [[SegmentInfo]] -> H.Html
getMapScript paths = do
        H.script ! A.type_ "text/javascript" ! A.src "http://maps.googleapis.com/maps/api/js?key=AIzaSyCtiE9l_Rhk7LNF-ImN5TJlkKTZWfL46XM&sensor=false" $ ""
        H.script ! A.type_ "text/javascript" $ H.preEscapedToMarkup mapscript  
    where
    mapscript = header ++ path ++ "}"
    path = snd $ flip execState (0, "") $
        forM_ paths $ \track -> modify (mkpath track)

    getSiMmxy s (minx, maxx, miny, maxy) = (min minx posx, max maxx posx, min miny posy, max maxy posy)
        where Position posx posy = siStart s 
    getMmxy (minx1, maxx1, miny1, maxy1) (minx2, maxx2, miny2, maxy2) 
        = (min minx1 minx2, max maxx1 maxx2, min miny1 miny2, max maxy1 maxy2)
    center = Position ((maxx + minx) / 2) ((maxy + miny) / 2)
        where
        (minx, maxx, miny, maxy) = foldr1 getMmxy $ map (foldr getSiMmxy (2 * pi, -2 * pi, 2 * pi, -2 * pi)) paths

    printPosition :: Position -> String
    printPosition (Position x y) = printf "new google.maps.LatLng(%f, %f)" (x / pi * 180) (y / pi * 180)

    toInt :: Double -> Int
    toInt x = fst $ properFraction x

    setMarker :: Int -> String -> String -> String -> Position -> String
    setMarker num title icon infotext pos = printf "    var markerPos%d = %s;  \n\  
\    var marker%d = new google.maps.Marker({   \n\
\      position: markerPos%d,  \n\
\      map: map,  \n\
\      icon: '%s',\n\
\      title:\"%s\"  \n\
\    });\n\
\    google.maps.event.addListener(marker%d, 'click', function() { \n\
\      info.setContent('%s'); info.open(map, marker%d);\n\
\    });\n" num (printPosition pos) num num icon title num infotext num

    printPath :: Int -> String -> [Position] -> String
    printPath num color points = coords ++ vardescr
        where
        prefix = "path" ++ show num
        coords = concat [printf "var %sCoordinates = [" prefix, intercalate "," pointlist, "];\n"]
        pointlist :: [String]
        pointlist = map printPosition points
        vardescr = printf "  var %s = new google.maps.Polyline({ \
\    path: %sCoordinates,  \n\
\    strokeColor: \"%s\",  \n\
\    strokeWeight: 2  \n\
\  });  \n\
\  \n\
\  %s.setMap(map); \n\
\  google.maps.event.addListener(%s, 'click', pathClickEvent);\n" prefix prefix color prefix prefix

    mkpath :: [SegmentInfo] -> (Int, String) -> (Int, String)
    mkpath track (num, text) = (num + 1, concat [text, route, marker])
        where
        (icon, title, color, infotext, points) = case siType (head track) of
            Lift -> ("/static/skilift.png", "Lift " ++ show num, "#FF0000", liftinfo,
                [siStart $ head track, siEnd $ last track])
            _ -> ("/static/snowboarding.png", "Track " ++ show num, "#0000FF", trackinfo,
                siStart (head track) : map siEnd track) 
        route = printPath num color points
        marker = setMarker num title icon infotext (siStart (head track))
        phm = formatTime defaultTimeLocale "%X"
        stime = siTime $ head track
        etime = siTime $ last track
        tracktime = siDuration (last track) + timeDelta etime stime
        tracklen = sum (map siDistance track)
        liftinfo = printf "Lift %d: %s - %s (%d min)<br>Length %.2f km, altitude change %.0f m" 
            num (phm stime) (phm etime) (toInt tracktime `div` 60)
            (sum (map siDistance track) / 1000) (sum (map siVDiff track)) 
        trackinfo = printf "Track %d: %s - %s (%d h %d min %d sec)<br>Length %.2f km, altitude drop %.0f m<br>\
                \Max speed %.1f km/h, average speed %.1f km/h<br>\
                \Max sustained speed %.1f km/h" 
            num (phm stime) (phm etime) (toInt tracktime `div` 3600) 
            (toInt tracktime `rem` 3600 `div` 60) (toInt tracktime `rem` 60)
            (tracklen / 1000) (negate $ sum $ map siVDiff track)
            (3.6 * maximum (map siSpeed track)) (3.6 * tracklen / tracktime) 
            (3.6 * maxSustSpeed 10 track)
    
    summary :: String
    summary = printf "Total track time %d h %d min %d sec<br>Total distance %.1f km" hour minute sec dist
        where
        dist = sum (map (sum . map siDistance) paths) / 1000
        tracktime = siDuration (last $ last paths) + timeDelta (siTime $ last $ last paths) (siTime $ head $ head paths)
        hour = toInt tracktime `div` 3600 
        minute = toInt tracktime `rem` 3600 `div` 60
        sec = toInt tracktime `rem` 60
    
    header = printf "function initialize() {  \n\
\    var myLatlng = %s;  \n\
\    var myOptions = {  \n\
\      zoom: 14,  \n\
\      center: myLatlng,  \n\
\      mapTypeId: google.maps.MapTypeId.TERRAIN  \n\
\    }  \n\
\    var info = new google.maps.InfoWindow({ content: '%s' });  \n\
\    function pathClickEvent(event) {   \n\
\        info.setContent(event.latLng.toString()); info.open(map); info.setPosition(event.latLng);  \n\
\    };  \n\
\    var map = new google.maps.Map(document.getElementById(\"map_canvas\"), myOptions);\n\
\    var trackStart = %s;\n\
\    info.open(map); info.setPosition(trackStart);\n" (printPosition center) summary (printPosition $ siStart $ head $ head paths)

getMapPage :: [[SegmentInfo]] -> H.Html
getMapPage track = doc
    where
    doc = H.docTypeHtml $ do
        H.head $ do
            H.title "sample"
            H.meta ! A.name "viewport" ! A.content "initial-scale=1.0, user-scalable=no"
            H.style ! A.type_ "text/css" $ "html { height: 100% } body { height: 100%; margin: 0; padding: 0 } #map_canvas { height: 100% }"
            getMapScript track
        H.body ! A.onload "initialize()" $ H.div ! A.id "map_canvas" $ "" 
