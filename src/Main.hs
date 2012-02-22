module Main where

import qualified Data.ByteString.Lazy as ByteString
import Data.Maybe
import Data.Time (UTCTime, diffUTCTime)
import System.Environment
import Text.Printf

import Parse
import Track

isGoodLift :: [TrackPoint] -> Bool
isGoodLift [_] = False
isGoodLift track = speedmatch && dirmatch
    where
    pointpairs = zip track (tail track)
    getPos (TrackPoint _ pos _) = pos
    getTime (TrackPoint time _ _) = time

    avgazm = azimuth $ vincentyFormulae (getPos $ head track) (getPos $ last track)
    checkazm (p1, p2) = azmdiff < 0.1
        where
        azm = azimuth $ vincentyFormulae (getPos p1) (getPos p2)
        adiff = abs (azm - avgazm)
        azmdiff = if adiff < pi then adiff else 2 * pi - adiff
    dirmatch = all checkazm pointpairs

    totalTime = realToFrac (diffUTCTime (getTime $ last track) (getTime $ head track))
    avgspeed = trackLength track / totalTime
    checkspeed (p1, p2) = rspdiff < 0.1 || aspdiff < 1
        where
        tdiff = realToFrac (diffUTCTime (getTime p2) (getTime p1))
        speed = if tdiff /= 0 then directDistance p1 p2 / tdiff else 0
        aspdiff = abs (speed - avgspeed)
        rspdiff = aspdiff / avgspeed
    speedmatch = all checkspeed pointpairs

data LiftState = LiftState { stCurrent :: [TrackPoint], stLifts :: [[TrackPoint]] }

findLift :: Int -> [TrackPoint] -> Maybe Int
findLift _ [] = Nothing
findLift offset track@(_:xs)
    | isGoodLift (take 5 track) = Just offset
    | otherwise                 = findLift (offset + 1) xs             

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

mergeLifts :: [[TrackPoint]] -> [[TrackPoint]]
mergeLifts [] = error "p2"
mergeLifts [_] = error "p1"
mergeLifts [t,_] = [t]
mergeLifts (track1:pause:track2:rest)
-- if time between tracks is short and speeds are almost same - merge tracks
    | tdiff < 60 && abs (spd1 - spd2) / spd1 < 0.2   = mergeLifts (combined : rest)
    | otherwise     = track1 : mergeLifts (track2 : rest)
    where
    combined = track1 ++ pause ++ track2
    getTime (TrackPoint time _ _) = time
    t1stime = getTime $ head track1
    t1etime = getTime $ last track1
    t2stime = getTime $ head track2
    t2etime = getTime $ last track2
    tdiff = realToFrac (diffUTCTime t2stime t1etime)
    time1 = realToFrac (diffUTCTime t1etime t1stime)
    time2 = realToFrac (diffUTCTime t2etime t2stime)
    spd1 = trackLength track1 / time1
    spd2 = trackLength track2 / time2

longLift :: [TrackPoint] -> Bool
longLift track = totalTime > 30
    where
    getTime (TrackPoint time _ _) = time
    totalTime = realToFrac (diffUTCTime (getTime $ last track) (getTime $ head track))

data PointInfo = PointInfo { pitime :: UTCTime, piSpeed :: Double, piAzimuth :: Double, piDistance :: Double, piVShift :: Double } 

makeTrackInfo :: [TrackPoint] -> [PointInfo]
makeTrackInfo track = zipWith makePoint track (tail track)
    where
    makePoint (TrackPoint time1 pos1 alt1) (TrackPoint time2 pos2 alt2) =
            PointInfo time1 speed azm dist vshift
        where
        PointShift dist azm = vincentyFormulae pos1 pos2
        vshift = alt2 - alt1
        timediff = realToFrac (diffUTCTime time2 time1)
        speed
            | timediff == 0     = 0 -- error ("teleport at " ++ show currtime)
            | otherwise         = dist / timediff

printPoint :: PointInfo -> IO String
printPoint (PointInfo time speed azm dist vshift) 
    = printf "%.1f\t%.2f\t%.1f\t%.1f\t%s\n" (speed * 3.6) azm dist vshift (show time)

printLift :: [TrackPoint] -> IO String
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
    let lifts = filter longLift $ (mergeLifts . getLifts) track
    let liftdist = sum $ map trackLength lifts
    let points = makeTrackInfo track
    
    _ <- printf "Total distance %.2f km\n" (trackLength track / 1000)
    case mode of
        "lifts" -> do
            _ <- printf "Lift distance %.2f km\n" (liftdist / 1000)
            mapM_ printLift lifts
        "track" -> do
            _ <- printf "speed\tazm\tdist\tvshift\n"
            mapM_ printPoint points
        _ ->
            print "invalid command"