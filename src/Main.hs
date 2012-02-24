module Main where

import qualified Data.ByteString.Lazy as ByteString
import Data.Time (UTCTime, diffUTCTime)
import Data.Maybe
import Data.List
import System.Environment
import Text.Printf

import Parse
import Track

import Debug.Trace

data VS = VS { vsAzimuth :: Double, vsDirectDistance :: Double, vsSpeed :: Double }
    deriving (Show)

getShift :: TrackPoint -> TrackPoint -> VS
getShift p1 p2 = VS azimuth distance speed
    where
    TrackPoint time1 pos1 alt1 = p1
    TrackPoint time2 pos2 alt2 = p2
    PointShift landdist azimuth = vincentyFormulae pos1 pos2
    altdist = alt2 - alt1
    distance = sqrt (altdist ^ (2 :: Int) + landdist ^ (2 :: Int))
    time = realToFrac (diffUTCTime time2 time1)
    speed = distance / time

isGoodLift :: [TrackPoint] -> Bool
isGoodLift [_] = False
isGoodLift track = trace (show (speedStdDev / avgspeed) ++ " " ++ show azmdev ++ " " ++ show (tpTime $ head track)) $ speedmatch && dirmatch
    where
    stddev :: [Double] -> Double
    stddev diffs = sqrt (sum (map (^(2::Int)) diffs) / fromIntegral (length diffs))
    azmdiff :: Double -> Double -> Double
    azmdiff a1 a2
        | diff > pi     = 2 * pi - diff
        | diff < (-pi)  = 2 * pi + diff
        | otherwise     = diff
        where diff = a2 - a1

    params = zipWith getShift track (tail track)

    totalTime = realToFrac (diffUTCTime (tpTime $ last track) (tpTime $ head track))
    avgspeed = sum (map vsDirectDistance params) / totalTime
    speedStdDev = stddev $ map (\x -> avgspeed - vsSpeed x) params
    speedmatch = (speedStdDev / avgspeed) < 0.3

    avgazm = psAzimuth $ vincentyFormulae (tpPos $ head track) (tpPos $ last track)
    azmdev = stddev $ map (azmdiff avgazm . vsAzimuth) params
    dirmatch = azmdev < 0.1

data LiftState = LiftState { stCurrent :: [TrackPoint], stLifts :: [[TrackPoint]] }

findLift :: [TrackPoint] -> ([TrackPoint], [TrackPoint])
findLift track
    | length track < 5 || isNothing idx     = ([], [])
    | goodstart                             = (firstmin, drop (length firstmin) track)
    | otherwise                             = findLift $ tail track
    where
    goodstart = isGoodLift firstmin
    minute :: TrackPoint -> Bool
    minute point = realToFrac (diffUTCTime (tpTime point) (tpTime $ head track)) >= (60.0 :: Double)
    idx = findIndex minute track
    firstmin = take (max 5 (1 + fromJust idx)) track

expandLift :: ([TrackPoint], [TrackPoint]) -> ([TrackPoint], [TrackPoint])
expandLift (lift, track)
    | null track || null lift   = (lift, track)
    | isGoodLift newlift        = expandLift (newlift, rest)
    | otherwise                 = (lift, track)
    where
    (point:rest) = track
    newlift = lift ++ [point]

firstLift :: [TrackPoint] -> ([TrackPoint], [TrackPoint])
firstLift track = expandLift $ findLift track

getLifts :: [TrackPoint] -> [[TrackPoint]]
getLifts track
    | null lift     = []
    | otherwise     = lift : getLifts rest
    where
    (lift, rest) = firstLift track

{--

getLifts :: [TrackPoint] -> [[TrackPoint]]
getLifts track = stLifts $ findNext LiftState { stCurrent = tstart, stLifts = [] } trest
    where
    lift_track = drop (fromJust $ findLift 0 track) track
    (tstart, trest) = splitAt 5 lift_track

    findNext :: LiftState -> [TrackPoint] -> LiftState
    findNext s [] = s
    findNext s points@(x:xs) = findNext newstate newtrack
        where
        newlift = stCurrent s ++ [x]
        stillgood = isGoodLift newlift
        n = findLift 0 (x:xs)
        (pause, cont) = splitAt (fromJust n) points
        (start, rest) = splitAt 5 cont 
        newstate
            | stillgood     = s { stCurrent = newlift }
            | isNothing n   = s
            | otherwise = LiftState { stCurrent = start, stLifts = stLifts s ++ [stCurrent s, pause]}
        newtrack
            | stillgood     = xs
            | isNothing n   = []
            | otherwise     = rest

--}

data PointInfo = PointInfo UTCTime Double Double Double Double Double 

makeTrackInfo :: [TrackPoint] -> [PointInfo]
makeTrackInfo track = zipWith makePoint track (tail track)
    where
    makePoint (TrackPoint time1 pos1 alt1) (TrackPoint time2 pos2 alt2) =
            PointInfo time1 speed azm alt1 dist vshift
        where
        PointShift dist azm = vincentyFormulae pos1 pos2
        vshift = alt2 - alt1
        timediff = realToFrac (diffUTCTime time2 time1)
        speed
            | timediff == 0     = 0 -- error ("teleport at " ++ show currtime)
            | otherwise         = dist / timediff

printPoint :: PointInfo -> IO String
printPoint (PointInfo time speed azm alt dist vshift) 
    = printf "%.1f\t%.2f\t%.0f\t%.1f\t%.1f\t%s\n" (speed * 3.6) azm alt dist vshift (show time)

printLift :: [TrackPoint] -> String
printLift l = printf "%d\t%s\t%s\n" (length l) (show htime) (show ltime)
    where
    getTime (TrackPoint time _ _) = time
    htime = getTime $ head l
    ltime = getTime $ last l 

main::IO()
main = do
    [mode, filename] <- take 2 `fmap` getArgs
    xml <- ByteString.readFile filename
    let track = parseTrack xml
    let lifts = getLifts track
    let liftdist = sum $ map trackLength lifts
    let points = makeTrackInfo track
    
    _ <- printf "Total distance %.2f km\n" (trackLength track / 1000)
    case mode of
        "lifts" -> do
            _ <- printf "Lift distance %.2f km\n" (liftdist / 1000)
            mapM_ (printf . printLift) lifts
        "track" -> do
            _ <- printf "speed\tazm\talt\tdist\tvshift\n"
            mapM_ printPoint points
        _ ->
            print "invalid command"