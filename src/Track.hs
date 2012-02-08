module Track (Position(Position), TrackPoint(TrackPoint)) where

import Data.Time (UTCTime)

data Position = Position Double Double  -- Lat Long
    deriving (Show)
data TrackPoint = TrackPoint UTCTime Position Double -- Altitude
    deriving (Show)
    