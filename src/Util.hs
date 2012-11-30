module Util where

import qualified Data.Foldable as F

import Data.Ratio ((%))
import Data.Time (UTCTime, diffUTCTime)

timeDelta :: UTCTime -> UTCTime -> Double
timeDelta t1 t2 = realToFrac (diffUTCTime t1 t2)

sq :: Num a => a -> a
sq n = n ^ (2 :: Int)

round2mm :: Double -> Double
round2mm x = fromRational (round (x * 1000) % 1000)    

stddev :: F.Foldable a => a Double -> Double
stddev values = sqrt (qsum / count)
    where
    (qsum, count) = F.foldr (\v (s, n) -> (s + sq v, n + 1)) (0, 0) values
