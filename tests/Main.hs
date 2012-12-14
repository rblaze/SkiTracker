module Main where

import Test.Framework(defaultMain)

import TestGeo
import TestPaint

main :: IO()
main = defaultMain $ geoTests ++ paintTests
