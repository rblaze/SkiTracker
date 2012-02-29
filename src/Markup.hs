module Markup where

import Prelude hiding (foldr, sum)

import Data.Foldable (Foldable, foldr, sum)
import Data.Time (UTCTime, diffUTCTime)

import qualified Queue as Q
import Track

data SegmentType = Idle | Track | Lift deriving (Enum, Show, Eq)
data SegmentInfo = SegmentInfo { siTime :: UTCTime, siType :: SegmentType, siStart :: Position,
        siEnd :: Position, siAlt :: Double, siAzimuth :: Double, siHDiff :: Double, siVDiff :: Double,
        siDistance :: Double, siDuration :: Double, siSpeed :: Double } 

timeDelta :: UTCTime -> UTCTime -> Double
timeDelta t1 t2 = realToFrac (diffUTCTime t1 t2)

makeTrackInfo :: [TrackPoint] -> [SegmentInfo]
makeTrackInfo track = zipWith mkinfo track (tail track)
    where
    mkinfo :: TrackPoint -> TrackPoint -> SegmentInfo
    mkinfo (TrackPoint time1 pos1 alt1) (TrackPoint time2 pos2 alt2)
        = SegmentInfo time1 Idle pos1 pos2 alt1 azimuth hdist vdiff totaldist timediff speed
        where
        PointShift hdist azimuth = vincentyFormulae pos1 pos2
        vdiff = alt2 - alt1
        totaldist = sqrt (hdist ^ (2 :: Int) + vdiff ^ (2 :: Int))
        timediff = timeDelta time2 time1
        speed = totaldist / timediff

isGoodLift :: Q.Queue SegmentInfo -> Bool
isGoodLift track
    | Q.length track < 5    = False 
    | otherwise             = speedmatch && dirmatch
    where
--    dbgstring :: String
--    dbgstring = printf "%0.2f %0.2f %s %s" (speedStdDev / avgspeed) azmdev (show (siTime $ Q.head track)) (show (siTime $ Q.last track))
    stddev :: Foldable a => a Double -> Double
    stddev diffs = sqrt (qsum / len)
        where
        (qsum, len) = foldr f (0, 0) diffs
        f v (s, l) = (s + v ^ (2 :: Int), l + 1) 
    azmdiff :: Double -> Double -> Double
    azmdiff a1 a2
        | diff > pi     = 2 * pi - diff
        | diff < (-pi)  = 2 * pi + diff
        | otherwise     = diff
        where diff = a2 - a1

    totalTime = sum (fmap siDuration track)
    avgspeed = sum (fmap siDistance track) / totalTime
    speedStdDev = stddev $ fmap (\x -> avgspeed - siSpeed x) track
    speedmatch = (speedStdDev / avgspeed) < 0.3

    avgazm = psAzimuth $ vincentyFormulae (siStart $ Q.head track) (siEnd $ Q.last track)
    azmdev = stddev $ fmap (azmdiff avgazm . siAzimuth) track
    dirmatch = azmdev < 0.14

fillInterval :: Double -> Q.Queue SegmentInfo -> [SegmentInfo] -> (Q.Queue SegmentInfo, [SegmentInfo])
fillInterval _ start [] = (start, []) 
fillInterval duration start rest@(x:xs)
    | Q.length start >= 5 && timeDelta (siTime $ Q.last start) (siTime $ Q.head start) >= duration
                    = (start, rest)
    | otherwise     = fillInterval duration (Q.push start x) xs

findLift :: [SegmentInfo] -> ([SegmentInfo], Q.Queue SegmentInfo, [SegmentInfo])
findLift track = (reverse pr, li, re)
    where
    (pr, li, re) = step [] Q.empty track 
    step :: [SegmentInfo] -> Q.Queue SegmentInfo -> [SegmentInfo] -> ([SegmentInfo], Q.Queue SegmentInfo, [SegmentInfo])
    step p l r
        | isGoodLift lift   = (p, lift, rest)
        | Q.null lift      = (p, lift, rest)
        | otherwise         = step (Q.head lift : p) (Q.pop lift) rest
        where
        (lift, rest) = fillInterval 60 l r

expandLift :: Q.Queue SegmentInfo -> [SegmentInfo] -> (Q.Queue SegmentInfo, [SegmentInfo])
expandLift lift track
    | Q.null lift || null track = (lift, track)
    | isGoodLift newlift        = expandLift newlift rest
    | otherwise                 = (lift, track)
    where
    (point:rest) = track
    newlift = Q.push lift point

markLifts :: [SegmentInfo] -> [SegmentInfo]
markLifts [] = []
markLifts track = intro ++ exlift ++ markLifts rest 
    where
    (intro, lift', rest') = findLift track
    (lift, rest) = expandLift lift' rest'
    exlift = map (\x -> x { siType = Lift }) $ Q.toList lift