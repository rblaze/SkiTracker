module Main where

import qualified Data.ByteString.Lazy as ByteString
import Control.Monad (liftM)
import Data.Time (UTCTime, diffUTCTime)
import System.Environment
import Text.Printf

import TCX
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

getTime :: TrackPoint -> UTCTime
getTime x = timeX
    where
    TrackPoint timeX _ _ = x

main::IO()
main = do
    xml <- head `fmap` getArgs >>= ByteString.readFile
    let track = parseTCX xml
    let speeds = trackSpeed track
    let times = map getTime track
    let maxspeed = maximum speeds
    let maxsust = maximum $ sustainedSpeed 10 [] (zip times speeds)
    
    _ <- printf "Total distance %.2f km\n" (trackLength track / 1000)
    _ <- printf "Max speed %.1f km/h\n" (maxspeed * 3.6)
    printf "Max sustained speed %.1f km/h\n" (maxsust * 3.6)