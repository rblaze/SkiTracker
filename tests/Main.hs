module Main where

import Test.Framework(defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
-- import Test.QuickCheck
import qualified MinQueue

import Data.Maybe
import Debug.Trace

main :: IO()
main = defaultMain tests
  where tests = [
            testGroup "MinQueue" [
                testCase "Empty Queue" testEmptyQueue,
                testCase "Emptied Queue" testEmptiedQueue,
                testProperty "LIFO" testLIFO,
                testProperty "Short LIFO" testShortLIFO,
                testProperty "Minimum" testMin
            ]
         ]

type QState = (MinQueue.MinQueue Int, [Int])

stateMove :: QState -> QState
stateMove (q, vals) = ( MinQueue.pop q, vals ++ [fromJust $ MinQueue.peek q])

stateEmpty :: QState -> Bool
stateEmpty (q, _) = MinQueue.null q

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
        popped = snd $ until stateEmpty stateMove (queue, [])

testShortLIFO :: Int -> [Int] -> Bool
testShortLIFO len xs = xs == popped
    where
        realLen = abs len `mod` (1 + length xs)
        (prexs, lastxs) = splitAt realLen xs
        preq = foldl (flip MinQueue.push) MinQueue.empty prexs
        pushpop :: QState -> Int -> QState
        pushpop (queue, out) v = (queue', out')
            where
                tq = MinQueue.push v queue
                out' = out ++ [fromJust $ MinQueue.peek tq]
                queue' = MinQueue.pop tq
        state = foldl pushpop (preq, []) lastxs
        popped = snd $ until stateEmpty stateMove state
        
testMin :: [Int] -> Bool
testMin xs = (null xs && isNothing (MinQueue.minimum queue)) || fromJust (MinQueue.minimum queue) == minimum xs
    where
        queue = foldl (flip MinQueue.push) MinQueue.empty xs
