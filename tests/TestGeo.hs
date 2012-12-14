module TestGeo (geoTests) where

import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Data.Time
import qualified Test.Framework as TF

import Geo
import SegmentedTrack
import Track

newtype TestPos = TestPos Position deriving Show

instance Arbitrary TestPos where
    arbitrary = do
        rlat <- choose(-90.0, 90.0)
        rlong <- choose(-180.0, 180.0)
        let lat = if abs rlat == 90.0 then 0.0 else rlat
        let long = if abs rlong == 180.0 then 0.0 else rlong
        return (TestPos $ Position (g2r lat) (g2r long))

geoTests :: [TF.Test]
geoTests = [
        TF.testGroup "Vincenty" [
            testCase "FlindersPeak - Buninyong" testVinc1,
            testCase "Moscow - StPetersburg" testVinc2,
            testCase "Moscow - Johannesburg" testVinc3,
            testCase "StPetersburg - Rio de Janeiro" testVinc4,
            testCase "Zero distance" testVinc5,
            testCase "Equator" testVinc6,
            testCase "Direct FlindersPeak - Buninyong" testVinc7,
            testCase "Direct Moscow - StPetersburg" testVinc8,
            testCase "Direct Moscow - Johannesburg" testVinc9,
            testCase "Direct StPetersburg - Rio de Janeiro" testVinc10,
            testCase "Direct Zero distance" testVinc11,
            testCase "Direct Equator" testVinc12,
            testProperty "Direct - Inverse" testDirectInverseProp
        ],
        TF.testGroup "Distance" [
            testProperty "Vertical" testDistVert,
            testProperty "Horizontal" testDistHoriz
        ]
    ]

zeroTime :: UTCTime
zeroTime = UTCTime (ModifiedJulianDay 0) 0

vincentyDistance :: Position -> Position -> Double
vincentyDistance p1 p2 = dvDistance (vincentyFormulae p1 p2)

almostEqual :: (Fractional f, Ord f) => f -> f -> Bool
almostEqual x y = abs (x - y) < 0.0000001

testDistVert :: TestPos -> Double -> Double -> Property
testDistVert (TestPos pos) alt altdiff =
        (alt > -500.0 && alt < 10000.0) ==>
        (altdiff > -3000.0 && alt < 3000.0) ==>
        ts3Ddistance seg - abs altdiff < 0.001
    where
        p1 = TrackPoint zeroTime pos alt
        p2 = TrackPoint zeroTime pos (alt + altdiff)
        seg = head $ makeSegmentedTrack [p1, p2]

testDistHoriz :: TestPos -> TestPos -> Double -> Property
testDistHoriz (TestPos p1) (TestPos p2) alt =
        (alt > -500.0 && alt < 10000.0) ==>
        ts3Ddistance seg == vincentyDistance p1 p2
    where
        tp1 = TrackPoint zeroTime p1 alt
        tp2 = TrackPoint zeroTime p2 alt
        seg = head $ makeSegmentedTrack [tp1, tp2]

testDirectInverseProp :: TestPos -> TestPos -> Property
testDirectInverseProp (TestPos source) (TestPos dest@(Position dlat dlong))
            = printTestCase (show rlat ++ " " ++ show rlong) (almostEqual dlat rlat && almostEqual dlong rlong)
    where
    vec = vincentyFormulae source dest
    Position rlat rlong = vincentyFormulae' source vec

g2r :: Double -> Double
g2r x = x * pi / 180

testVinc1 :: Assertion
testVinc1 = vincentyDistance flindersPeak buninyong @?= 54972.271
    where
    flindersPeak = Position (g2r $ -37.95103341666666666666) (g2r 144.42486788888888888888)
    buninyong = Position (g2r $ -37.65282113888888888888) (g2r 143.92649552777777777777)
    
testVinc2 :: Assertion
testVinc2 = vincentyDistance moscow stpetersburg @?= 633278.594
    where
    moscow = Position (g2r 55.755837) (g2r 37.617683)
    stpetersburg = Position (g2r 59.930950) (g2r 30.361865)
    
testVinc3 :: Assertion
testVinc3 = vincentyDistance moscow johannesburg @?= 9125078.802
    where
    moscow = Position (g2r 55.755837) (g2r 37.617683)
    johannesburg = Position (g2r $ -26.191295) (g2r 28.030198)

testVinc4 :: Assertion
testVinc4 = vincentyDistance stpetersburg rio @?= 11312892.565
    where
    stpetersburg = Position (g2r 59.930950) (g2r 30.361865)
    rio = Position (g2r $ -22.904483) (g2r $ -43.209071)

testVinc5 :: Assertion
testVinc5 = vincentyDistance rio rio @?= 0
    where
    rio = Position (g2r $ -22.904483) (g2r $ -43.209071)

testVinc6 :: Assertion
testVinc6 = vincentyDistance p1 p2 @?= 55478.459
    where
    p1 = Position 0 (g2r 144.424867)
    p2 = Position 0 (g2r 143.92649552)

testDirectInverse :: Position -> Position -> Assertion
testDirectInverse source dest@(Position dlat dlong)
            = assertBool (show dlong ++ " " ++ show rlong) $ almostEqual dlat rlat && almostEqual dlong rlong
    where
    vec = vincentyFormulae source dest
    Position rlat rlong = vincentyFormulae' source vec

testVinc7 :: Assertion
testVinc7 = testDirectInverse flindersPeak buninyong
    where
    flindersPeak = Position (g2r $ -37.9510334166) (g2r 144.42486788)
    buninyong = Position (g2r $ -37.65282113888) (g2r 143.926495527777)

testVinc8 :: Assertion
testVinc8 = testDirectInverse moscow stpetersburg
    where
    moscow = Position (g2r 55.755837) (g2r 37.617683)
    stpetersburg = Position (g2r 59.930950) (g2r 30.361865)
    
testVinc9 :: Assertion
testVinc9 = testDirectInverse moscow johannesburg
    where
    moscow = Position (g2r 55.755837) (g2r 37.617683)
    johannesburg = Position (g2r $ -26.191295) (g2r 28.030198)

testVinc10 :: Assertion
testVinc10 = testDirectInverse stpetersburg rio
    where
    stpetersburg = Position (g2r 59.930950) (g2r 30.361865)
    rio = Position (g2r $ -22.904483) (g2r $ -43.209071)

testVinc11 :: Assertion
testVinc11 = testDirectInverse rio rio
    where
    rio = Position (g2r $ -22.904483) (g2r $ -43.209071)

testVinc12 :: Assertion
testVinc12 = testDirectInverse p1 p2
    where
    p1 = Position 0 (g2r 144.424867)
    p2 = Position 0 (g2r 143.92649552)
