module SegmentStats(sustainedSpeed) where

import Data.Foldable as F
import Data.Sequence (ViewL(..), (|>))

import SegmentedTrack
import qualified Data.Sequence as Q

type Q = Q.Seq TrackSegment

-- sustained time
minInterval :: Double
minInterval = 10

qSpeed :: Q -> Double
qSpeed q = F.minimum $ fmap ts3Dspeed q

segSpeed :: Q -> [TrackSegment] -> [Double]
segSpeed q [] = [qSpeed q]
segSpeed q (x:xs) | Q.null q = segSpeed (Q.singleton x) xs
segSpeed q (x:xs) | F.sum (fmap tsDuration q) < minInterval = segSpeed (q |> x) xs
segSpeed q (x:xs) = let _ :< rest = Q.viewl q
                     in qSpeed q : segSpeed (rest |> x) xs

sustainedSpeed :: [TrackSegment] -> Double
sustainedSpeed seg = F.maximum $ segSpeed Q.empty seg
