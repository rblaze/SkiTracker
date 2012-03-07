module Main where

import qualified Data.ByteString.Lazy as BS
import Data.List (groupBy)
import Data.Function (on)
import System.Environment
import Text.Blaze.Renderer.Pretty
import Text.Printf

import AppServer
import Parse
import Markup
import Maps

--import Debug.Trace

printSegment :: SegmentInfo -> IO String
printSegment p 
    = printf "%.1f\t%.2f\t%.0f\t%.1f\t%.1f\t%s\n" (siSpeed p * 3.6) (siAzimuth p) (siAlt p) (siDistance p) (siVDiff p) (show $ siTime p)

printLift :: [SegmentInfo] -> String
printLift l = printf "%d\t%s\t%s\n" (length l) (show $ siTime (head l)) (show $ siTime (last l))

localMain :: IO ()
localMain = do
    [mode, filename] <- take 2 `fmap` getArgs
    xml <- BS.readFile filename
    let gpstrack = parseTrack xml
    let track = makeTrackInfo gpstrack
    let marked = markLifts track
    let parts = groupBy ((==) `on` siType) marked
    let lifts = filter (\x -> siType (head x) == Lift) parts
    let liftdist = sum $ map (sum . fmap siDistance) lifts
    
    _ <- printf "Total distance %.2f km\n" $ sum (map siDistance track) / 1000
    case mode of
        "lifts" -> do
            _ <- printf "Lift distance %.2f km\n" (liftdist / 1000)
            mapM_ (printf . printLift) lifts
            let html = getMapPage parts
            writeFile "track.html" $ renderHtml html
--            pic <- Maps.getMap lifts
--            ByteString.writeFile "mypic.png" pic
        "track" -> do
            _ <- printf "speed\tazm\talt\tdist\tvshift\n"
            mapM_ printSegment track
        _ ->
            print "invalid command"

main::IO()
main = do
    mode <- head `fmap` getArgs
    case mode of
        "appserver" -> appServerMain
        _ -> localMain
