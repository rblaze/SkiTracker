module Main where

import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Maybe
import Data.Time (parseTime)
import System.Environment
import System.Locale (defaultTimeLocale)
import Text.Blaze.Html.Renderer.Pretty
import Text.Printf

import HtmlMap
import Parse
import SegmentedTrack
import PaintSki
import Geo

printSegment :: TrackSegment -> String
printSegment s = intercalate "\t" [printf "%.2f" $ ts3Dspeed s,
        printf "%.1f" (57.2957 * dvAzimuth (tsVector s)),
        show $ tsStartTime s, show $ tsDuration s,
        show startLat, show startLong, show $ tsStartAlt s,
        show endLat, show endLong, show $ tsEndAlt s,
        show $ dvDistance $ tsVector s, show $ dvAzimuth $ tsVector s]
    where
        (Position startLat startLong) = tsStartPos s
	(Position endLat endLong) = tsEndPos s

printSegmentWithType :: TrackSegment -> String
printSegmentWithType s = typechar ++ "\t" ++ printSegment s
    where
    typechar = case tsType s of
        Idle -> "I"
        Track -> "M"
        Lift -> "L"

readSegment :: String -> TrackSegment
readSegment s = TrackSegment typ stime dur spos salt epos ealt vec
    where
    [ctype, _, _, ssdate, sstime, _, sdur, stlat, stlong, ssalt, elat, elong, sealt, sdist, sazm] = words s
    typ = case ctype of
        "I" -> Idle
        "M" -> Track
        "L" -> Lift
        _   -> undefined
    stime = fromJust $ parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (ssdate ++ " " ++ sstime)
    [dur, salt, ealt] = map read [sdur, ssalt, sealt]
    spos = Position (read stlat) (read stlong)
    epos = Position (read elat) (read elong)
    vec = DistVector (read sdist) (read sazm)

main :: IO()
main = do
    [mode, filename] <- take 2 `fmap` getArgs
    case mode of
        "parse" -> do
            filedata <- BS.readFile filename
            let gpstrack = parseTrack filedata
            let segments = makeSegmentedTrack gpstrack

            mapM_ (printf "%s\n" . printSegment) segments
{--
        "paint" -> do
            filedata <- BS.readFile filename
            let gpstrack = parseTrack filedata
            let segments = makeSegmentedTrack gpstrack
            let colored = paintSkiTrack segments

            mapM_ (printf "%s\n" . printSegmentWithType) colored
--}
        "draw" -> do
            filedata <- BS.readFile filename
            let gpstrack = parseTrack filedata
            let segments = makeSegmentedTrack gpstrack
            let colored = paintSkiTrack segments

            printf "%s" (renderHtml $ makeMapPage colored)
{--
        "drawmarked" -> do
            filedata <- readFile filename
            let segments = map readSegment $ lines filedata

            printf "%s" (renderHtml $ makeMapPage segments)
--}
        _ -> printf "do not know what is %s\n" mode

