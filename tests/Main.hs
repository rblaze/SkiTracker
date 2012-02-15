module Main where

import Test.Framework(defaultMain, testGroup)
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck2
import Test.HUnit
-- import Test.QuickCheck

import Track

main :: IO()
main = defaultMain tests
    where 
    tests = [
        testGroup "Vincenty" [
            testCase "FlindersPeak - Buninyong" testVinc1,
            testCase "Moscow - StPetersburg" testVinc2,
            testCase "Moscow - Johannesburg" testVinc3,
            testCase "StPetersburg - Rio de Janeiro" testVinc4,
            testCase "Zero distance" testVinc5,
            testCase "Equator" testVinc6
        ]
     ]

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
    