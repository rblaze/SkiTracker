module Util where

import Data.Ratio ((%))
import Data.Time (UTCTime, diffUTCTime)

timeDelta :: UTCTime -> UTCTime -> Double
timeDelta t1 t2 = realToFrac (diffUTCTime t1 t2)

p2 :: Int
p2 = 2

round2mm :: Double -> Double
round2mm x = fromRational (round (x * 1000) % 1000)    
