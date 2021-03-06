{-# LANGUAGE OverloadedStrings #-}

module HtmlMap(makeMapPage, analyticsCode) where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.String as R
import Data.List
import Data.Time.Format
import Data.Time.Clock (UTCTime)
import System.Locale (defaultTimeLocale)
import Text.Blaze ((!))
import Text.Printf

import Geo
--import GoogleStaticMap
import PaintSki
import SegmentedTrack
import SegmentStats

data TrackStats = TrackStats {
        startDate :: String,
        startTime :: String,
        duration :: String,
        slopeTime :: String,
        aproxDistance :: String,
        slopeDistance :: String,
        nRuns :: Int,
        maxRunDistance :: String,
        maxRunTime :: String,
        maxSpeed :: String,
        maxSustainedSpeed :: String
    }

analyticsCode :: H.Html
analyticsCode = H.script ! A.type_ "text/javascript" $ H.toHtml ("var _gaq = _gaq || [];  \n\
\  _gaq.push(['_setAccount', 'UA-20054266-7']);  \n\
\  _gaq.push(['_trackPageview']);  \n\
\  \n\
\  (function() {  \n\
\    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;  \n\
\    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';  \n\
\    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);  \n\
\  })();" :: String)

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
printPosition (Position x y) = printf "new google.maps.LatLng(%0.6f, %0.6f)" (x / pi * 180) (y / pi * 180)

printSegment :: String -> [TrackSegment] -> String
printSegment name track = "    var " ++ name ++ " = [\n        "
        ++ intercalate ",\n        " (map (printPosition . tsStartPos) track)
        ++ ",\n        "
        ++ printPosition (tsEndPos $ last track) ++ "\n"
        ++ "     ];\n"

{--
printPolyLine :: String -> [Position] -> String
printPolyLine name line = "    var " ++ name
        ++ " = google.maps.geometry.encoding.encodePath([\n        "
        ++ intercalate ",\n        " (map printPosition line)
        ++ "\n     ]);\n"

setMarker :: String -> Position -> Int -> String -> String
setMarker iconfile pos uniq message = "    var " ++ posname ++ " = "
        ++ printPosition pos ++ ";\n\
\    var " ++ markname ++ " = new google.maps.Marker({\n\
\        position: " ++ posname ++ ",\n\
\        map: map,\n\
\        icon: '" ++ iconfile ++ "'\n\
\     });\n"
        ++ addClickListener markname message
    where
    posname = printf "markerPos%d" uniq
    markname = printf "marker%d" uniq
--}

{--
setStartMarker :: Int -> SkiRun -> String
setStartMarker uniq track = setMarker iconfile startpos uniq message
    where
    startpos = tsStartPos $ head $ runPoints track
    message = makeRunSummary track
    iconfile = "/static/" ++ iconname
    iconname = case runType track of
                Track   -> "snowboarding.png"
                Lift    -> "skilift.png"
                Idle    -> "rest.png"
--}

addClickListener :: String -> String -> String
addClickListener object message =
        "    google.maps.event.addListener(" ++ object
        ++ ", 'click', function(event) "
        ++ "{ onPathClick(event, \"" ++ message ++  "\");});\n"

addSegmentToMap :: Int -> SkiRun -> String
addSegmentToMap uniq track = printSegment pathname (runPoints track)
        ++ polyline linename pathname
        ++ "    " ++ linename ++ ".setMap(map);\n"
        ++ addClickListener linename message
{--
        ++ (case runType track of
                Idle -> setStartMarker uniq track
                Lift -> setStartMarker uniq track
                _ -> ""
            )
--}
    where
    message = makeRunSummary track
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
    H.script ! A.type_ "text/javascript" ! A.src (H.toValue $ "http://maps.googleapis.com/maps/api/js?key=" ++ googleApiKey ++ "&libraries=geometry&sensor=false") $ ""
    H.script ! A.type_ "text/javascript" $ H.toHtml script
--    H.script ! A.type_ "text/javascript" $ H.toHtml staticscript
    where
    script :: String
    script = header ++ path ++ footer

    header = "/****** begin script header *******/\n\
\function initialize() {  \n\
\    var centerPoint = " ++ printPosition (centerPoint track) ++ ";  \n\
\    var myOptions = {  \n\
\      zoom: 14,  \n\
\      center: centerPoint,  \n\
\      mapTypeId: google.maps.MapTypeId.TERRAIN  \n\
\    };  \n\
\    var map = new google.maps.Map(document.getElementById(\"map_canvas\"), myOptions);\n\
\    function onPathClick(event, message) {   \n\
\        document.getElementById(\"segdata\").innerHTML = message;    \n\
\    };  \n\
\/******* end script header *******/\n"

    footer = "}"
    path = concat $ zipWith addSegmentToMap [0..] track

{--
    staticmap = stripMap track
    staticpath = printPolyLine "staticline" $ concatMap plPoints staticmap
    staticurl = "        var mapurl = 'http://maps.googleapis.com/maps/api/staticmap?path=enc:' + staticline + '&sensor=false&size=400x400';"

    staticscript :: String
    staticscript = staticpath ++ staticurl
--}

styleSheet :: H.Html
styleSheet = "\
\        html, body {    \n\
\            margin:0;    \n\
\            padding:0;    \n\
\            height:100%; /* needed for container min-height */    \n\
\        }    \n\
\        div#container {    \n\
\            height:100%;    \n\
\            width:100%;    \n\
\            height:auto !important;    \n\
\            margin:0 auto;    \n\
\            min-height:100%;    \n\
\            overflow:hidden;    \n\
\            position:relative;    \n\
\        }    \n\
\        div#leftCol {    \n\
\            left:0;    \n\
\            bottom:0;    \n\
\            top:0;    \n\
\            overflow:hidden;    \n\
\            position:absolute;    \n\
\            width:20em;    \n\
\        }    \n\
\        div#content {    \n\
\            right:0;    \n\
\            bottom:0;    \n\
\            top:0;    \n\
\            left: 20em;    \n\
\            overflow:hidden;    \n\
\            position:absolute;    \n\
\        }    \n\
\        .paddedContent {    \n\
\            height:100%;    \n\
\        }"

printDuration :: Double -> String
printDuration d
    | hours > 0 = if mins /= 0 then printf "%dh %dm" hours mins else printf "%dh" hours
    | secs /= 0 = printf "%dm %ds" mins secs
    | otherwise = printf "%dm" mins
    where
    (ms, secs) = quotRem (round d :: Int) 60
    (hours, mins) = quotRem ms 60

printDistance :: Double -> String
printDistance d
    | km > 0    = if m /= 0 then printf "%dkm %dm" km m else printf "%dkm" km
    | otherwise = printf "%dm" m
    where (km, m) = quotRem (round d :: Int) 1000

printSpeed :: Double -> String
printSpeed s = printf "%0.1f km/h" (s * 3.6)

printTime :: String -> UTCTime -> String
printTime = formatTime defaultTimeLocale

makeRunSummary :: SkiRun -> String
makeRunSummary track = R.renderHtml $ sequence_ $ intersperse H.br $ map H.toHtml $
        case runType track of
            Idle -> [ "Idle " ++ printTime "%X" (runStartTime track),
                      "Duration: " ++ printDuration (runDuration track),
                      "Avg Speed: " ++ printSpeed (runAvgSpeed track)
                    ]
            Lift -> [ "Lift " ++ printTime "%X" (runStartTime track),
                      "Duration: " ++ printDuration (runDuration track),
                      "Avg Speed: " ++ printSpeed (runAvgSpeed track)
                    ]
            Track -> [ "Run " ++ printTime "%X" (runStartTime track),
                       "Duration: " ++ printDuration (runDuration track),
                       "Length: " ++ printDistance (runDistance track),
                       "Avg speed: " ++ printSpeed (runAvgSpeed track),
                       "Max speed: " ++ printSpeed (maximum $ map tsSpeed $ runPoints track),
                       "Max sustained speed: " ++ printSpeed (sustainedSpeed $ runPoints track)
                    ]

makeTrackSummary :: [SkiRun] -> TrackStats
makeTrackSummary track = TrackStats {
        startDate = printTime "%x" $ runStartTime firstSegment,
        startTime = printTime "%X" $ runStartTime firstSegment,
        duration = printDuration (sum $ map runDuration track),
        slopeTime = printDuration (sum $ map runDuration runs),
        slopeDistance = printDistance rundist,
        aproxDistance = printDistance $ fromIntegral (1000 * round (rundist / 1000) :: Int),
        nRuns = length runs,
        maxRunDistance = printDistance (maximum $ map runDistance runs),
        maxRunTime = printDuration (maximum $ map runDuration runs),
        maxSpeed =  printSpeed (maximum $ map tsSpeed $ concatMap runPoints runs),
        maxSustainedSpeed = printSpeed (maximum $ map (sustainedSpeed . runPoints) runs)
    }
    where
    rundist = sum $ map runDistance runs
    firstSegment = head track
    runs = filter (\s -> runType s == Track) track

printTrackSummary :: TrackStats -> H.Html
printTrackSummary track = sequence_ $ intersperse H.br $ map H.toHtml
    [
        "Track date: " ++ startDate track,
        "Start time: " ++ startTime track,
        "Total time: " ++ duration track,
        "Slope time: " ++ slopeTime track,
        "Total " ++ slopeDistance track ++ " in " ++ show (nRuns track) ++ " slope runs",
        "Max run distance: " ++ maxRunDistance track,
        "Max run time: " ++ maxRunTime track,
        "Max speed: " ++ maxSpeed track,
        "Max sustained speed: " ++ maxSustainedSpeed track
      ]

initFacebook :: String -> TrackStats -> H.Html
initFacebook uri stats = do
    H.div ! A.id "fb-root" $ ""
    H.script ! A.type_ "text/javascript" ! A.src "http://connect.facebook.net/en_US/all.js" $ ""
    H.script ! A.type_ "text/javascript" $ H.toHtml $ "    \n\
\        FB.init({appId: '400355043379044', status: true, cookie: true});    \n\
\        function postToFeed() {    \n\
\            // calling the API ...    \n\
\            var obj = {    \n\
\                method: 'feed',    \n\
\                display: 'popup',    \n\
\                link: '" ++ uri ++ "',    \n\
\                picture: 'http://skitracker.ruddy.ru/static/snowboarding.png', \n\
\                name: '" ++ aproxDistance stats ++ " down slope!',    \n\
\                description: 'Total time " ++ duration stats ++ ", slope time " ++ slopeTime stats
                        ++ ", max speed " ++ maxSpeed stats ++ ", sustained speed " ++ maxSustainedSpeed stats ++ "'    \n\
\            };    \n\
\            function callback(response) {    \n\
\                document.getElementById('segdata').innerHTML = 'Post ID: ' + response['post_id'];    \n\
\            }    \n\
\            FB.ui(obj, callback);    \n\
\        }"

makeMapPage :: String -> [SkiRun] -> H.Html
makeMapPage uri track = do
    let stats = makeTrackSummary track
    H.docTypeHtml $ do
        H.head $ do
            H.title "Track map"
            H.meta ! A.name "viewport" ! A.content "initial-scale=1.0, user-scalable=no"
            H.style ! A.type_ "text/css" $ styleSheet
            generateScript track

        H.body ! A.onload "initialize()" $ do
            initFacebook uri stats
            H.div ! A.id "container" $ do
                H.div ! A.id "leftCol" $ H.div ! A.class_ "paddedContent" $ do
                    H.div ! A.id "trackdata" ! A.style "height: 50%" $ do
                        printTrackSummary stats
                        H.br
                        H.a ! A.onclick "postToFeed(); return false;" $ H.img ! A.src "/static/post-button.png"
                        H.p "Click on track to see segment details"
                    H.div ! A.id "segdata" $ ""
                H.div ! A.id "content" $ H.div ! A.class_ "paddedContent" $
                    H.div ! A.id "map_canvas" ! A.style "width: 100%; height: 100%" $ ""
            analyticsCode
