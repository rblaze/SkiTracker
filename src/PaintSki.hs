module PaintSki(SkiRun(..), paintSkiTrack) where

import Data.Foldable (Foldable(..), toList)
import Data.Function
import Data.List(groupBy)
import Data.Sequence (ViewL(..), ViewR(..), (|>))
import Data.Time(UTCTime)

import qualified Data.Foldable as F
import qualified Data.Sequence as Q

import SegmentedTrack
import Geo
import Util

data SkiRun = SkiRun {
    runType      :: SegmentType,
    runAvgSpeed  :: Double,
    runDuration  :: Double,
    runDistance  :: Double,
    runStartTime :: UTCTime,
    runPoints    :: [TrackSegment]
  }

type LookAhead = Q.Seq TrackSegment

minLiftDuration :: Num a => a
minLiftDuration = 60

minSegmentTime :: Num a => a
minSegmentTime = 30

minLiftDistance :: Num a => a
minLiftDistance = 50

liftSpeedVariance :: Fractional a => a
liftSpeedVariance = 0.23

liftAzimuthDev :: Fractional a => a
liftAzimuthDev = 0.14

segDuration :: (Functor a, Foldable a) => a TrackSegment -> Double
segDuration q = F.sum $ tsDuration `fmap` q

segDistance :: (Functor a, Foldable a) => a TrackSegment -> Double
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

    shead :< _ = Q.viewl seg
    _ :> slast = Q.viewr seg

    straightLine = vincentyFormulae (tsStartPos shead) (tsEndPos slast)
    straightAzimuth = dvAzimuth straightLine
    segmentAzimuth = dvAzimuth . tsVector
    azimuthStddev = stddev $ fmap (azimuthDiff straightAzimuth . segmentAzimuth) seg

    vdiff = tsEndAlt slast - tsStartAlt shead

    hasStableSpeed = speedVariance < liftSpeedVariance
    hasStableDirection = azimuthStddev < liftAzimuthDev

paintLowSpeed :: [TrackSegment] -> [TrackSegment]
paintLowSpeed = map (\s -> s{tsType = if ts3Dspeed s < 1.0 then Idle else Track })

paintLifts :: LookAhead -> [TrackSegment] -> [TrackSegment]
paintLifts liftq []
    | not $ validLiftData liftq = toList liftq
    | not $ isGoodLift liftq    = toList liftq
    | otherwise                 = toList $ setType Lift liftq
paintLifts liftq (s:ss)
    | not $ validLiftData liftq = paintLifts (liftq |> s) ss
    | not $ isGoodLift liftq    = let qhead :< rest = Q.viewl liftq
                                   in qhead : paintLifts (rest |> s) ss
    | otherwise                 = tryExpandLift liftq s ss

tryExpandLift :: LookAhead -> TrackSegment -> [TrackSegment] -> [TrackSegment]
tryExpandLift liftq s ss
    | isGoodLift nextq = paintLifts nextq ss
    | otherwise        = repainted ++ paintLifts Q.empty ss
    where
    nextq = liftq |> s
    repainted = toList $ setType Lift liftq

mergeHead :: [[TrackSegment]] ->  [[TrackSegment]]
mergeHead track = if null rest then track else (mhead ++ r):rs
    where
    (small, rest@(r:rs)) = break (\s -> segDuration s > minSegmentTime) track
    mhead = setType (tsType $ head r) $ concat small

mergeMiddle :: [[TrackSegment]] -> [[TrackSegment]]
mergeMiddle [] = []
mergeMiddle track@(x1:_) = func (tsType $ head x1) [] track
    where
    func :: SegmentType -> [[TrackSegment]] -> [[TrackSegment]] -> [[TrackSegment]]
    -- common case, nothing to do, just go further
    func _ [] (x:xs)
        | segDuration x > minSegmentTime    = x : func (tsType $ head x) [] xs
    -- end with long segment
    func _ [] [] = []
    -- end with short segment
    func ptype hold [] = map (setType ptype) $ reverse hold
    -- inside short run, end or continue
    func ptype hold (x:xs)
        | segDuration x > minSegmentTime
                    = map (setType htype) (reverse hold) ++ x : func xtype [] xs
        | otherwise = func ptype (x:hold) xs
        where
        xtype = tsType $ head x
        htype = if ptype == xtype then ptype else Idle

annotate :: [TrackSegment] -> SkiRun
annotate s = SkiRun (tsType $ head s) avg duration distance (tsStartTime $ head s) s
    where
    duration = segDuration s
    distance = segDistance s
    avg = distance / duration

paintSkiTrack :: [TrackSegment] -> [SkiRun]
paintSkiTrack track = filtered
    where
    painted = paintLifts Q.empty $ paintLowSpeed track
    filtered = map annotate $ regroup $ mergeMiddle $ mergeHead $ regroup [painted]
    regroup = groupBy ((==) `on` tsType) . concat
