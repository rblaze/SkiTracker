module Maps(getMapPage) where

import Control.Monad.State
import Data.List (intercalate)
import Data.Time.Format(formatTime)
import System.Locale (defaultTimeLocale)
import Text.Printf

import Markup
import Track
import Util

getMapPage :: [[SegmentInfo]] -> String
getMapPage paths = header ++ path ++ footer
    where
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

    setMarker :: Int -> String -> Maybe String -> Maybe String -> Position -> String
    setMarker num title icon infotext pos = printf "    var markerPos%d = %s;  \n\  
\    var marker%d = new google.maps.Marker({   \n\
\      position: markerPos%d,  \n\
\      map: map,  \n\
\      %s\
\      title:\"%s\"  \n\
\  });\n\
\  %s" num (printPosition pos) num num icontext title event
        where
        icontext = maybe "" (printf "icon: \'%s\',\n") icon
        event = maybe "" (\s -> printf "google.maps.event.addListener(marker%d, 'click', function() { \
            \ info.setContent('%s'); info.open(map, marker%d); });  \n" num s num) infotext

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
\  %s.setMap(map); \n" prefix prefix color prefix

    mkpath :: [SegmentInfo] -> (Int, String) -> (Int, String)
    mkpath track (num, text) = (num + 1, text ++ route ++ marker)
        where
        (icon, title, color, infotext, points) = case siType (head track) of
            Lift -> (Just "skilift.png", "Lift " ++ show num, "#FF0000",
                Just liftinfo,
                [siStart $ head track, siEnd $ last track])
            _ -> (Just "snowboarding.png", "Track " ++ show num, "#0000FF",
                Just trackinfo,
                siStart (head track) : map siEnd track) 
        route = printPath num color points
        marker = setMarker num title icon infotext (siStart (head track))
        phm = formatTime defaultTimeLocale "%X"
        stime = siTime $ head track
        etime = siTime $ last track
        tracktime :: Int
        tracktime = fst $ properFraction (timeDelta etime stime + siDuration (last track))
        liftinfo = printf "Lift %d: %s - %s (%d min)<br>Length %.2f km, altitude change %.0f m" 
            num (phm stime) (phm etime) (tracktime `div` 60)
            (sum (map siDistance track) / 1000) (sum (map siVDiff track)) 
        trackinfo = printf "Track %d: %s - %s (%d h %d min %d sec)<br>Length %.2f km, altitude drop %.0f m" 
            num (phm stime) (phm etime) (tracktime `div` 3600) (tracktime `rem` 3600 `div` 60) (tracktime `rem` 60)
            (sum (map siDistance track) / 1000) (negate $ sum $ map siVDiff track) 
    header = printf "<!DOCTYPE html>\n\
\<html>  \n\
\<head>  \n\
\<title>Example: Simple</title>  \n\
\<meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\"/>  \n\
\<meta name=\"viewport\" content=\"initial-scale=1.0, user-scalable=no\" />  \n\
\<style type=\"text/css\">  \n\
\  html { height: 100%% }  \n\
\  body { height: 100%%; margin: 0; padding: 0 }  \n\
\  #map_canvas { height: 100%% }  \n\
\</style>  \n\
\<script type=\"text/javascript\"  \n\
\  src=\"http://maps.googleapis.com/maps/api/js?key=AIzaSyCtiE9l_Rhk7LNF-ImN5TJlkKTZWfL46XM&sensor=false\">  \n\
\</script>  \n\
\<script type=\"text/javascript\">  \n\
\  function initialize() {  \n\
\    var myLatlng = %s;  \n\
\    var myOptions = {  \n\
\      zoom: 14,  \n\
\      center: myLatlng,  \n\
\      mapTypeId: google.maps.MapTypeId.TERRAIN  \n\
\    }  \n\
\    var info = new google.maps.InfoWindow({});  \n\
\    var map = new google.maps.Map(document.getElementById(\"map_canvas\"), myOptions);  \n" (printPosition center)

    footer = "  }  \n\
\</script>  \n\
\</head>  \n\
\<body onload=\"initialize()\">  \n\
\  <div id=\"map_canvas\"></div>  \n\
\</body>  \n\
\</html>"

