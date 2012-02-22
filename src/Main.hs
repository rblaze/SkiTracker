module Main where

import qualified Data.ByteString.Lazy as ByteString
import Data.Time (UTCTime, diffUTCTime)
import System.Environment
import Text.Printf

import Parse
import Track

isGoodLift :: [TrackPoint] -> Bool
isGoodLift [_] = True
isGoodLift track = speedmatch && dirmatch
    where
    seqPairs xs = zip xs (tail xs)
    getPos (TrackPoint _ pos _) = pos
    getTime (TrackPoint time _ _) = time
    avgazm = azimuth $ vincentyFormulae (getPos $ head track) (getPos $ last track)
    checkazm (p1, p2) = azmdiff < 0.1
        where
        azm = azimuth $ vincentyFormulae (getPos p1) (getPos p2)
        adiff = abs (azm - avgazm)
        azmdiff = if adiff < pi then adiff else 2 * pi - adiff
    dirmatch = all checkazm $ seqPairs track
    totalTime = realToFrac (diffUTCTime (getTime $ last track) (getTime $ head track))
    avgspeed = trackLength track / totalTime
    checkspeed (p1, p2) = spdiff < 0.1
        where
        tdiff = realToFrac (diffUTCTime (getTime p2) (getTime p1))
        speed = if tdiff /= 0 then directDistance p1 p2 / tdiff else 0
        spdiff = abs (speed - avgspeed) / avgspeed
    speedmatch = all checkspeed $ seqPairs track

data LiftState = LiftState { stCurrent :: [TrackPoint], stLifts :: [[TrackPoint]] }

findLift :: [TrackPoint] -> ([TrackPoint], [TrackPoint])
findLift track = splitAt 5 stripped 
    where
    stripped = until (isGoodLift . take 5) tail track

get' :: LiftState -> [TrackPoint] -> LiftState
get' s [] = s
get' s (x:xs) = get' newstate track
    where
    newtrack = stCurrent s ++ [x]
    stillgood = isGoodLift newtrack
    (start, rest) = findLift (x:xs)
    newstate
        | stillgood = s { stCurrent = newtrack }
        | otherwise = LiftState { stCurrent = start, stLifts = stCurrent s : stLifts s }
    track = if stillgood then xs else rest

getLifts :: [TrackPoint] -> [[TrackPoint]]
getLifts track = stLifts $ get' LiftState { stCurrent = start, stLifts = [[]] } rest
    where
    (start, rest) = findLift track

printLift :: [TrackPoint] -> String
printLift l = show (length l) ++ " " ++ show l

main::IO()
main = do
    xml <- head `fmap` getArgs >>= ByteString.readFile
    let track = parseTrack xml
    
    _ <- printf "Total distance %.2f km\n" (trackLength track / 1000)
    mapM_ (print . printLift) $ getLifts track