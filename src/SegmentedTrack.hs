module SegmentedTrack(SegmentType(..), TrackSegment(..),
        makeSegmentedTrack, ts3Dspeed, tsSpeed, tsAltDiff) where

import Data.Time (UTCTime)

import Geo
import Track
import Util

data SegmentType = Idle | Track | Lift
        deriving (Enum, Show, Eq)

data TrackSegment = TrackSegment {
        tsType :: SegmentType,
        tsStartTime :: UTCTime,
        tsDuration :: Double,
        tsStartPos :: Position,
        tsStartAlt :: Double,
        tsEndPos :: Position,
        tsEndAlt :: Double,
        tsVector :: DistVector
    }

tsAltDiff :: TrackSegment -> Double
tsAltDiff ts = tsEndAlt ts - tsStartAlt ts

tsSpeed :: TrackSegment -> Double
tsSpeed ts = dvDistance (tsVector ts) / tsDuration ts

ts3Dspeed :: TrackSegment -> Double
ts3Dspeed ts = sqrt (sq (dvDistance (tsVector ts)) + sq (tsAltDiff ts)) / tsDuration ts

mkSegment :: TrackPoint -> TrackPoint -> TrackSegment
mkSegment pt1 pt2 = TrackSegment { tsType = undefined,
        tsStartTime = tpTime pt1, tsDuration = timeDelta (tpTime pt2) (tpTime pt1),
        tsStartPos = tpPos pt1, tsStartAlt = tpAlt pt1,
        tsEndPos = tpPos pt2, tsEndAlt = tpAlt pt2,
        tsVector = vincentyFormulae (tpPos pt1) (tpPos pt2)
    }

makeSegmentedTrack :: [TrackPoint] -> [TrackSegment]
makeSegmentedTrack ts = zipWith mkSegment ts (tail ts)
