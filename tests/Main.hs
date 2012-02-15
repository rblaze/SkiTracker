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
            testCase "FlindersPeak - Buninyong" testVinc1
        ]
     ]

testVinc1 :: Assertion    
testVinc1 = vincentyDistance flindersPeak buninyong @?= 54972.271
    where
    g2r x = x * pi / 180
    flindersPeak = Position (g2r $ -37.95103341666666666666) (g2r 144.42486788888888888888)
    buninyong = Position (g2r $ -37.65282113888888888888) (g2r 143.92649552777777777777)