module SegmentStats(sustainedSpeed) where

import Data.Foldable as F

import SegmentedTrack
import qualified Queue as Q

type Q = Q.Queue TrackSegment

-- sustained time
minInterval :: Double
minInterval = 10

qSpeed :: Q -> Double
qSpeed q = F.minimum $ fmap ts3Dspeed q

segSpeed :: Q -> [TrackSegment] -> [Double]
segSpeed q [] = [qSpeed q]
segSpeed q (x:xs) | Q.null q = segSpeed (Q.push q x) xs
segSpeed q (x:xs) | (F.sum $ fmap tsDuration q) < minInterval = segSpeed (Q.push q x) xs
segSpeed q (x:xs) = qSpeed q : segSpeed (Q.pop $ Q.push q x) xs

sustainedSpeed :: [TrackSegment] -> Double
sustainedSpeed seg = F.maximum $ segSpeed Q.empty seg
