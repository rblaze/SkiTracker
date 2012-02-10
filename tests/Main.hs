module Main where

import Test.Framework(defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
-- import Test.QuickCheck
import qualified MinQueue

import Data.Maybe

main :: IO()
main = defaultMain tests
  where tests = [
            testGroup "MinQueue" [
                testCase "Empty Queue" testEmptyQueue,
                testCase "Emptied Queue" testEmptiedQueue,
                testProperty "LIFO" testLIFO,
                testProperty "Minimum" testMin
            ]
         ]

testEmptyQueue :: Assertion
testEmptyQueue = isNothing v @?= True
    where
        v = MinQueue.peek (MinQueue.empty :: MinQueue.MinQueue Int) 

testEmptiedQueue :: Assertion
testEmptiedQueue = isNothing v @?= True
    where
        v = MinQueue.peek emptyq
        emptyq = MinQueue.pop $ MinQueue.pop dataq
        dataq = MinQueue.push 1 $ MinQueue.push 2 (MinQueue.empty :: MinQueue.MinQueue Int)

testLIFO :: [Int] -> Bool
testLIFO xs = xs == popped
    where
        queue = foldl (flip MinQueue.push) MinQueue.empty xs
        pp :: ([Int], MinQueue.MinQueue Int) -> ([Int], MinQueue.MinQueue Int)
        pp (vals, q) = ( vals ++ [fromJust $ MinQueue.peek q], MinQueue.pop q)
        emp :: ([Int], MinQueue.MinQueue Int) -> Bool
        emp (_, q) = MinQueue.null q
        popped = fst $ until emp pp ([], queue)

testMin :: [Int] -> Bool
testMin xs = (null xs && isNothing (MinQueue.minimum queue)) || fromJust (MinQueue.minimum queue) == minimum xs
    where
        queue = foldl (flip MinQueue.push) MinQueue.empty xs
