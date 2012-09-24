module Markup(SegmentType(..), SegmentInfo(..), makeTrackInfo, markLifts, maxSustSpeed) where

import Prelude hiding (foldr, sum)

import Data.Foldable (Foldable, foldr, sum)
import Data.Time (UTCTime)

import qualified Queue as Q
import Track
import Util

--import Text.Printf

data SegmentType = Idle | Track | Lift deriving (Enum, Show, Eq)
data SegmentInfo = SegmentInfo { siTime :: UTCTime, siType :: SegmentType, siStart :: Position,
        siEnd :: Position, siAlt :: Double, siAzimuth :: Double, siHDiff :: Double, siVDiff :: Double,
        siDistance :: Double, siDuration :: Double, siSpeed :: Double } 

makeTrackInfo :: [TrackPoint] -> [SegmentInfo]
makeTrackInfo track = zipWith mkinfo track (tail track)
    where
    mkinfo :: TrackPoint -> TrackPoint -> SegmentInfo
    mkinfo (TrackPoint time1 pos1 alt1) (TrackPoint time2 pos2 alt2)
        = SegmentInfo time1 Idle pos1 pos2 alt1 azimuth hdist vdiff totaldist timediff speed
        where
        DistVector hdist azimuth = vincentyFormulae pos1 pos2
        vdiff = alt2 - alt1
        totaldist = sqrt (hdist ^ (2 :: Int) + vdiff ^ (2 :: Int))
        timediff = timeDelta time2 time1
        speed = totaldist / timediff

isGoodLift :: Q.Queue SegmentInfo -> Bool
isGoodLift track
    | Q.null track          = False
            -- little hack for GPS losing signal
            -- any interval longer than 1 minute with more then 60 m altitude gain counts as lift
    | Q.length track < 5    = vdiff > 60
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

    avgazm = dvAzimuth $ vincentyFormulae (siStart $ Q.head track) (siEnd $ Q.last track)
    azmdev = stddev $ fmap (azmdiff avgazm . siAzimuth) track
    dirmatch = azmdev < 0.14
    
    vdiff = siVDiff (Q.last track) + siAlt (Q.last track) - siAlt (Q.head track)

fillInterval :: Int -> Double -> Q.Queue SegmentInfo -> [SegmentInfo] -> (Q.Queue SegmentInfo, [SegmentInfo])
fillInterval _ _ start [] = (start, []) 
fillInterval minlen duration start rest@(x:xs)
    | not (Q.null start) && siDuration (Q.last start) + timeDelta (siTime $ Q.last start) (siTime $ Q.head start) >= duration
                    = (start, rest)
    | otherwise     = fillInterval minlen duration (Q.push start x) xs

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
        (lift, rest) = fillInterval 5 90 l r

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

maxSustSpeed :: Double -> [SegmentInfo] -> Double
maxSustSpeed duration track = maximum (getMins Q.empty track)
    where
    minspeed :: Q.Queue SegmentInfo -> Double
    minspeed = foldr (\a b -> min (siSpeed a) b) 1000 -- unlikely there will be hypersonic speeds
    getMins :: Q.Queue SegmentInfo -> [SegmentInfo] -> [Double]
    getMins _ [] = []
    getMins q tr = minspeed fillq : getMins (Q.pop fillq) rest
        where (fillq, rest) = fillInterval 1 duration q tr
