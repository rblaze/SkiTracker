module Track (TrackPoint(..)) where

import Data.Time (UTCTime)
import Geo

data TrackPoint = TrackPoint { tpTime :: UTCTime, tpPos :: Position, tpAlt :: Double }
    deriving (Show, Eq)
