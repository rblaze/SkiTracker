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

data TVA = TVA UTCTime PointShift Double Double
    deriving (Show)

trackVector :: [TrackPoint] -> [TVA]
trackVector [] = []
trackVector [_] = []
trackVector (x:xs) = TVA time1 shift (speed * 3.6) dalt : trackVector xs
    where
    TrackPoint time1 pos1 alt1 = x
    TrackPoint time2 pos2 alt2 = head xs
    shift = vincentyFormulae pos1 pos2
    speed = directDistance x (head xs) / timediff
    dalt = alt2 - alt1
    timediff
        | time1 /= time2    = realToFrac (diffUTCTime time2 time1)
        | otherwise         = 0.5

printTVA :: TVA -> String
printTVA t = printf "%.1f\t%.2f\t%.1f\t%.1f\t%s\n" speed (azm * 180 / pi) dist dalt (show time)
    where
    TVA time (PointShift dist razm) speed dalt = t
    azm
        | razm >= 0     = razm
        | otherwise     = 2 * pi + razm

data Avg = Avg UTCTime Double Double Double Double
    deriving (Show)

trackDiff :: [TrackPoint] -> [Avg]
trackDiff (a:rest@(b:c:d:e:f:_)) = Avg ftime avgspeed avgazm speeddiff azmdiff : trackDiff rest
    where
    TrackPoint atime apos _ = a
    TrackPoint etime epos _ = e
    TrackPoint ftime fpos _ = f
    avgspeed = (directDistance a b + directDistance b c +
        directDistance c d + directDistance d e) / timeae
    timeae = realToFrac (diffUTCTime etime atime)
    avgazm = azimuth $ vincentyFormulae apos epos
    timefe = realToFrac (diffUTCTime ftime etime)
    speedfe = if timefe /= 0 then directDistance e f / timefe else 0
    speeddiff = abs (speedfe - avgspeed) / avgspeed
    azmfe = azimuth $ vincentyFormulae epos fpos
    adiff = abs (azmfe - avgazm)
    azmdiff = if adiff < pi then adiff else 2 * pi - adiff
trackDiff _ = []

printDiff :: Avg -> String
printDiff t = printf "%c%.1f\t%.2f\t%.2f\t%.2f\t%s\n" mark (aspeed * 3.6) (azm * 180 / pi) dspeed dazm (show time)
    where
    Avg time aspeed aazm dspeed dazm = t
    azm
        | aazm >= 0     = aazm
        | otherwise     = 2 * pi + aazm
    mark = if dspeed < 0.1 && dazm < 0.175 then '*' else ' '

main::IO()
main = do
    xml <- head `fmap` getArgs >>= ByteString.readFile
    let track = parseTrack xml
--    let tvas = trackVector track
    let diffs = trackDiff track
    
    _ <- printf "Total distance %.2f km\n" (trackLength track / 1000)
--    mapM_ (printf . printTVA) tvas
    mapM_ (printf . printDiff) diffs
