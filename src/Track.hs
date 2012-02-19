module Track (Position(Position), TrackPoint(TrackPoint), vincentyDistance, directDistance,
        trackLength, trackSpeed) where

import Data.List
import Data.Ratio ((%))
import Data.Time (UTCTime, diffUTCTime)
import Math.Sequence.Converge (convergeTo)

data Position = Position Double Double  -- Lat Long
    deriving (Show)
data TrackPoint = TrackPoint UTCTime Position Double -- Altitude
    deriving (Show)

p2 :: Int
p2 = 2

round2mm :: Double -> Double
round2mm x = fromRational (round (x * 1000) % 1000)    

directDistance :: TrackPoint -> TrackPoint -> Double
directDistance (TrackPoint _ pos1 alt1) (TrackPoint _ pos2 alt2) = round2mm $ sqrt (altdist ^ p2 + landdist ^ p2)
    where
    altdist = alt1 - alt2
    landdist = vincentyDistance pos1 pos2 

vincentyDistance :: Position -> Position -> Double
vincentyDistance (Position lat1 long1) (Position lat2 long2) 
    | lat1 == lat2 && long1 == long2    = 0 
    | otherwise                         = round2mm (b * al * (fin_sig - dsig))
    where
    a = 6378137.0       -- length of major axis of the ellipsoid (radius at equator)
    f = 1/298.257223563 -- flattening of the ellipsoid
    b = (1 - f) * a     -- length of minor axis of the ellipsoid (radius at the poles)
    u1 = atan ((1 - f) * tan lat1)
    u2 = atan ((1 - f) * tan lat2)
    ll = long2 - long1
    la_good = convergeTo 0 1e-12 (iterate stepla ll)
    (fin_cos2al, fin_sinsig, fin_cossig, fin_cossigm2, _, fin_sig) = precalc la_good 
    up2 = fin_cos2al * (a ^ p2 - b ^ p2) / (b ^ p2)
    al = 1 + up2 / 16384 * (4096 + up2 * (-768 + up2 * (320 - 175 * up2)))
    bl = up2 / 1024 * (256 + up2 * (-128 + up2 * (74 - 47 * up2)))
    dsig = bl * fin_sinsig * (fin_cossigm2 + bl / 4 * (fin_cossig * (-1 + 2 * (fin_cossigm2 ^ p2)) - bl / 6 * fin_cossigm2 * (-3 + 4 * (fin_sinsig ^ p2) * (-3 + 4 * (fin_cossigm2 ^ p2)))))

    precalc :: Double -> (Double, Double, Double, Double, Double, Double)
    precalc la = (cos2al, sinsig, cossig, cossigm2, sinal, sig)
        where
        sinsig = sqrt ((cos u2 * sin la) ^ p2 + (cos u1 * sin u2 - sin u1 * cos u2 * cos la) ^ p2)
        cossig = sin u1 * sin u2 + cos u1 * cos u2 * cos la
        sig = atan2 sinsig cossig
        sinal = cos u1 * cos u2 * sin la / sinsig
        cos2al = 1 - (sinal ^ p2)
        cossigm2 
            | cos2al == 0   = 0
            | otherwise     = cossig - 2 * sin u1 * sin u2 / cos2al

    stepla :: Double -> Double
    stepla la = ll + (1 - cl) * f * sinal * (sig + cl * sinsig * (cossigm2 + cl * cossig * (-1 + 2 * (cossigm2 ^ p2))))
        where
        (cos2al, sinsig, cossig, cossigm2, sinal, sig) = precalc la 
        cl = f / 16 * cos2al * (4 + f * (4 - 3 * cos2al))
        
trackLength :: [TrackPoint] -> Double
trackLength track = snd $ foldl step (head track, 0.0) track
    where
    step (prev, dist) point = (point, dist + directDistance prev point)
    
trackSpeed :: [TrackPoint] -> [Double]
trackSpeed track = snd $ mapAccumL step (head track) track
    where
    step prev point = (point, speed)
        where
        TrackPoint prevtime _ _ = prev
        TrackPoint currtime _ _ = point
        timediff = realToFrac (diffUTCTime currtime prevtime)
        distance = directDistance prev point
        speed
            | timediff == 0 && distance == 0    = 0
            | timediff == 0                     = distance / 1.0 -- error ("teleport at " ++ show currtime)
            | otherwise                         = distance / timediff
