module PaintSki(SkiRun(..), paintSkiTrack) where

import Data.Function
import Data.List(groupBy)
import Data.Time(UTCTime)

import qualified Queue as Q
import qualified Data.Foldable as F
import SegmentedTrack
import Geo
import Util

data SkiRun = SkiRun {
    runType      :: SegmentType,
    runAvgSpeed  :: Double,
    runDuration  :: Double,
    runStartTime :: UTCTime,
    runPoints    :: [TrackSegment]
  }

type LookAhead = Q.Queue TrackSegment

minLiftDuration :: Num a => a
minLiftDuration = 60

minLiftDistance :: Num a => a
minLiftDistance = 50

liftSpeedVariance :: Fractional a => a
liftSpeedVariance = 0.3

liftAzimuthDev :: Fractional a => a
liftAzimuthDev = 0.14

segDuration :: (Functor a, F.Foldable a) => a TrackSegment -> Double
segDuration q = F.sum $ tsDuration `fmap` q

segDistance :: (Functor a, F.Foldable a) => a TrackSegment -> Double
segDistance q = F.sum $ (dvDistance . tsVector) `fmap` q

setType :: Functor a => SegmentType -> a TrackSegment -> a TrackSegment
setType color = fmap (\s -> s{tsType = color})

validLiftData :: LookAhead -> Bool
validLiftData q
    | segDuration q < minLiftDuration    = False
    | otherwise                         = True

isGoodLift :: LookAhead -> Bool
isGoodLift seg
    | duration < minLiftDuration  = undefined -- catch invalid use
    -- little hack around GPS signal loss
    -- any interval longer than 1 minute with more then 60m altitude gain counts as lift
    | Q.length seg < 5 && vdiff > 60 = True
    | distance < minLiftDistance  = False
    | otherwise                   = hasStableSpeed && hasStableDirection
    where
    duration = segDuration seg
    distance = segDistance seg
    avgSpeed = distance / duration
    speedStddev = stddev $ fmap (\p -> ts3Dspeed p - avgSpeed) seg
    speedVariance = speedStddev / avgSpeed

    straightLine = vincentyFormulae (tsStartPos $ Q.head seg) (tsEndPos $ Q.last seg)
    straightAzimuth = dvAzimuth straightLine
    segmentAzimuth = dvAzimuth . tsVector
    azimuthStddev = stddev $ fmap (azimuthDiff straightAzimuth . segmentAzimuth) seg

    vdiff = tsEndAlt (Q.last seg) - tsStartAlt (Q.head seg)

    hasStableSpeed = speedVariance < liftSpeedVariance
    hasStableDirection = azimuthStddev < liftAzimuthDev

paintLowSpeed :: [TrackSegment] -> [TrackSegment]
paintLowSpeed = map (\s -> s{tsType = if ts3Dspeed s < 1.0 then Idle else Track })

paintLifts :: LookAhead -> [TrackSegment] -> [TrackSegment]
paintLifts liftq []
    | not $ validLiftData liftq = Q.toList liftq
    | not $ isGoodLift liftq    = Q.toList liftq
    | otherwise                 = Q.toList $ setType Lift liftq
paintLifts liftq (s:ss)
    | not $ validLiftData liftq = paintLifts (Q.push liftq s) ss
    | not $ isGoodLift liftq    = Q.head liftq : paintLifts (Q.pop $ Q.push liftq s) ss
    | otherwise                 = tryExpandLift liftq s ss

tryExpandLift :: LookAhead -> TrackSegment -> [TrackSegment] -> [TrackSegment]
tryExpandLift liftq s ss
    | isGoodLift nextq = paintLifts nextq ss
    | otherwise        = repainted ++ paintLifts Q.empty ss
    where
    nextq = Q.push liftq s
    repainted = Q.toList $ setType Lift liftq

filterShortSegments :: [[TrackSegment]] ->  [[TrackSegment]]
filterShortSegments (xp:x:xn:xs)
    | segDuration x > 30 = xp : filterShortSegments (x:xn:xs)
-- check times below
--    | tsType (head xp) == tsType (head xn) && segDuration xp > 30 = filterShortSegments (longseg : xs)
    | tsType (head xp) == Track && tsType (head x) == Idle && tsType (head xn) == Track = filterShortSegments (longseg : xs)
    | otherwise = xp : filterShortSegments (x:xn:xs)
    where
    longseg = xp ++ setType (tsType $ head xp) x ++ xn
filterShortSegments xs = xs

annotate :: [TrackSegment] -> SkiRun
annotate s = SkiRun (tsType $ head s) avg duration (tsStartTime $ head s) s
    where
    duration = segDuration s
    distance = segDistance s
    avg = distance / duration

paintSkiTrack :: [TrackSegment] -> [SkiRun]
paintSkiTrack track = filtered
    where
    painted = paintLifts Q.empty $ paintLowSpeed track
    filtered = map annotate $ filterShortSegments $ groupBy ((==) `on` tsType) painted
