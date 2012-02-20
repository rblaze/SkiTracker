module Main where

import qualified Data.ByteString.Lazy as ByteString
import Data.Time (UTCTime, diffUTCTime)
import System.Environment
import Text.Printf

import Parse
import Track

sustainedSpeed :: Double -> [(UTCTime, Double)] -> [(UTCTime, Double)] -> [Double]
sustainedSpeed _ _ [] = []
sustainedSpeed interval [] (x:xs) = sustainedSpeed interval [x] xs
sustainedSpeed interval state points@(x:xs)
    | timelen endTime < interval    = sustainedSpeed interval newstate xs
    | timelen prevTime >= interval  = sustainedSpeed interval (init state) points
    | otherwise                     = sustspeed : sustainedSpeed interval newstate xs 
    where
    timelen b = realToFrac $ diffUTCTime (fst x) b
    sustspeed = minimum $ snd $ unzip newstate
    newstate = x : state
    endTime = fst $ last state
    prevTime = fst $ if length state > 1 then last $ init state else x

sigChar :: Double -> Char
sigChar x
    | x < 0     = 'V'
    | x == 0    = '='
    | otherwise = '^'  

vfilter :: (TrackPoint, TrackPoint, Int, Double, Double) -> [(TrackPoint, Double)] -> [(Char, Int, Double, String, Double, Double, Double, Double)]
vfilter (start, end, count, tracklen, dir) track
    | null track                    = [ retpoint ]
    | signum dir == signum vspeed   = vfilter (start, point, count + 1, tracklen + seglen, dir) xs
    | otherwise                     = retpoint : vfilter (end, point, 1, seglen, vspeed) xs
    where
    retpoint = (sigChar dir, count, tracktime, show stime, ealt - salt, salt, tracklen, trackdirect)
    TrackPoint stime _ salt = start
    TrackPoint etime _ ealt = end
    ((point, vspeed) : xs) = track
    trackdirect = directDistance start end
    tracktime = realToFrac $ diffUTCTime etime stime
    seglen = directDistance end point

getTime :: TrackPoint -> UTCTime
getTime x = timeX
    where
    TrackPoint timeX _ _ = x

printRes :: (Char, Int, Double, String, Double, Double, Double, Double) -> String
printRes (dir, count, time, start, dalt, elev, tracklen, trackdirect) 
    = printf "%c %d\t%f\t%.1f\t%.0f\t%.2f\t%.2f\t%.1f\t%s\n" dir count time dalt elev tracklen trackdirect (tracklen * 3.6 / time) start

main::IO()
main = do
    xml <- head `fmap` getArgs >>= ByteString.readFile
    let track = parseTrack xml
    let speeds = trackSpeed track
    let times = map getTime track
    let maxspeed = maximum (fst $ unzip speeds)
    let maxsust = maximum $ sustainedSpeed 10 [] $ zip times (fst $ unzip speeds)
    let maxascentspeed = maximum $ snd $ unzip speeds 
    let maxdescentspeed = minimum $ snd $ unzip speeds
    
    _ <- printf "Total distance %.2f km\n" (trackLength track / 1000)
    _ <- printf "Max speed %.1f km/h\n" (maxspeed * 3.6)
    _ <- printf "Max sustained speed %.1f km/h\n" (maxsust * 3.6)
    _ <- printf "Max ascent speed %.1f m/s\n" maxascentspeed
    _ <- printf "Max descent speed %.1f m/s\n" (-maxdescentspeed)
    mapM_ (printf . printRes) $ vfilter (head track, head track, 0, 0, 0) $ zip track (snd $ unzip speeds)
