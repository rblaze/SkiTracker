module Track (Position(Position), TrackPoint(TrackPoint, tpTime, tpPos, tpAlt),
        PointShift(PointShift, psDistance, psAzimuth), 
        vincentyDistance, vincentyFormulae, directDistance, trackLength) where

import Data.Ratio ((%))
import Data.Time (UTCTime)
import Math.Sequence.Converge (convergeTo)

data Position = Position Double Double  -- Lat Long
    deriving (Show)
data TrackPoint = TrackPoint { tpTime :: UTCTime, tpPos :: Position, tpAlt :: Double }
    deriving (Show)
data PointShift = PointShift { psDistance :: Double, psAzimuth :: Double }
    deriving (Show)

p2 :: Int
p2 = 2

round2mm :: Double -> Double
round2mm x = fromRational (round (x * 1000) % 1000)    

vincentyFormulae :: Position -> Position -> PointShift
vincentyFormulae (Position lat1 long1) (Position lat2 long2) 
    | lat1 == lat2 && long1 == long2    = PointShift 0 0 
    | otherwise                         = PointShift (round2mm (b * al * (fin_sig - dsig))) azm
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
    azm = atan2 (cos u2 * sin la_good) (cos u1 * sin u2 - sin u1 * cos u2 * cos la_good)

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

vincentyDistance :: Position -> Position -> Double
vincentyDistance pos1 pos2 = psDistance (vincentyFormulae pos1 pos2)  

directDistance :: TrackPoint -> TrackPoint -> Double
directDistance (TrackPoint _ pos1 alt1) (TrackPoint _ pos2 alt2) = round2mm $ sqrt (altdist ^ p2 + landdist ^ p2)
    where
    altdist = alt1 - alt2
    landdist = vincentyDistance pos1 pos2 

trackLength :: [TrackPoint] -> Double
trackLength track = sum $ zipWith directDistance track (tail track)