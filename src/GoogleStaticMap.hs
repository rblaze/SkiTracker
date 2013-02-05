module GoogleStaticMap(stripMap, PolyLine(..)) where

import Data.Function
import Data.List (groupBy)

import PaintSki
import SegmentedTrack
import Geo

data PolyLine = PolyLine {
    plType :: SegmentType,
    plPoints :: [Position]
  } deriving Show

compressLifts :: [TrackSegment] -> PolyLine
compressLifts s
    | linetype == Lift  = PolyLine {
                            plType = linetype,
                            plPoints = [headpoint, tsEndPos $ last s]
                          }
    | otherwise         = PolyLine {
                            plType = linetype,
                            plPoints = headpoint : map tsEndPos s
                          }
    where
    linetype = tsType $ head s
    headpoint = tsStartPos $ head s

shrinkRuns :: PolyLine -> PolyLine
shrinkRuns s@PolyLine{plPoints = pts} = s{plPoints = drop2 pts}
    where
    drop2 :: [a] -> [a]
    drop2 (v1:_:vs@(_:_)) = v1:drop2 vs
    drop2 vs = vs

shrinkPath :: [PolyLine] -> [PolyLine]
shrinkPath s | length (concatMap plPoints s) < 150 = s
shrinkPath s = shrinkPath $ map shrinkRuns s

-- removes all Idle segments, compresses Lift segments to two points
stripMap :: [SkiRun] -> [PolyLine]
stripMap s = shrinkPath $ map compressLifts $ groupBy ((==) `on` tsType) $ filter (\ts -> tsType ts /= Idle) $ concatMap runPoints s
