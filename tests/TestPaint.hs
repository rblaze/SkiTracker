module TestPaint(paintTests) where

import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Time
import System.Random
import qualified Test.Framework as TF

import Geo
import SegmentedTrack
import PaintSki

paintTests :: [TF.Test]
paintTests = [
        TF.testGroup "SegmentMerge" [
            testCase "Merge Order" testMergeOrder
        ]
    ]

g2r :: Double -> Double
g2r x = x * pi / 180

testMergeOrder :: Assertion
testMergeOrder = assert $ all segEq $ zip source merged
    where
    segEq (t1, t2) = tsStartPos t1 == tsStartPos t2 && tsEndPos t1 == tsEndPos t2
    skirun = paintSkiTrack source
    merged = concatMap runPoints skirun
    startPos = Position (g2r 55.755837) (g2r 37.617683)
    startMove = DistVector 10 0.51
    start = TrackSegment {
            tsType = undefined,
            tsStartTime = UTCTime (fromGregorian 2012 12 21) (secondsToDiffTime $ 3600 * 12 + 187),
            tsDuration = 3,
            tsStartPos = startPos,
            tsStartAlt = 300,
            tsEndPos = vincentyFormulae' startPos startMove,
            tsEndAlt = 300,
            tsVector = startMove
        }
    moves = mkIdleSeq (mkStdGen 100) 600 40 
        ++ mkTrackSeq (mkStdGen 200) 150 50
        ++ mkIdleSeq (mkStdGen 300) 45 7
        ++ mkTrackSeq (mkStdGen 1) 10 3
        ++ mkIdleSeq (mkStdGen 2) 10 3
        ++ mkTrackSeq (mkStdGen 3) 12 3
        ++ mkIdleSeq (mkStdGen 4) 7 2
        ++ mkTrackSeq (mkStdGen 5) 14 3
        ++ mkIdleSeq (mkStdGen 300) 45 8
        ++ mkTrackSeq (mkStdGen 400) 180 70
        ++ mkIdleSeq (mkStdGen 500) 25 1
    source = scanl mkseg start moves
    mkseg seg (vec, time) = moveSeg seg time vec

mkSpeedSeq :: RandomGen g => Double -> Double -> g -> Int -> Int -> [(DistVector, Double)]
mkSpeedSeq _ _ _ _ 0 = []
mkSpeedSeq minspeed maxspeed gen time nseg = (vec, fromIntegral segtime) : mkSpeedSeq minspeed maxspeed gnext (time - segtime) (nseg-1)
    where
    (segtime, g1) = if nseg == 1 then (time, gen) else randomR (1, time - nseg + 1) gen
    (azimuth, g2) = randomR (-pi, pi) g1
    mindist = minspeed * fromIntegral segtime
    maxdist = maxspeed * fromIntegral segtime
    (dist, gnext) = randomR (mindist, maxdist) g2
    vec = DistVector dist azimuth

mkIdleSeq :: RandomGen g => g -> Int -> Int -> [(DistVector, Double)]
mkIdleSeq = mkSpeedSeq 0 0.999999

mkTrackSeq :: RandomGen g => g -> Int -> Int -> [(DistVector, Double)]
mkTrackSeq = mkSpeedSeq 1.0001 10

moveSeg :: TrackSegment -> Double -> DistVector -> TrackSegment
moveSeg seg time vec = seg {
        tsStartTime = addUTCTime (fromInteger $ floor $ tsDuration seg) (tsStartTime seg),
        tsDuration = time,
        tsStartPos = tsEndPos seg,
        tsEndPos = vincentyFormulae' (tsEndPos seg) vec,
        tsVector = vec
    }
