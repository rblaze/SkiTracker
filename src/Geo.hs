module Geo(Position(..), DistVector(..), vincentyFormulae, azimuthDiff) where

import Math.Sequence.Converge (convergeTo)

import Util

data Position = Position Double Double  -- Lat Long
    deriving (Show, Eq)
data DistVector = DistVector { dvDistance :: Double, dvAzimuth :: Double }
    deriving (Show)

-- ---------------
-- vincentyFormulae
-- | 'vincentyFormulae' @start end@ calculates distance and azimuth
-- from @start@ to @end@ on Earth's surface.
vincentyFormulae :: Position -> Position -> DistVector
vincentyFormulae (Position lat1 long1) (Position lat2 long2) 
    | lat1 == lat2 && long1 == long2    = DistVector 0 0 
    | otherwise                         = DistVector {dvDistance = round2mm (b * al * (fin_sig - dsig)), dvAzimuth = azm}
    where
    a = 6378137.0       -- length of major axis of the ellipsoid (radius at equator)
    f = 1/298.257223563 -- flattening of the ellipsoid
    b = (1 - f) * a     -- length of minor axis of the ellipsoid (radius at the poles)
    u1 = atan ((1 - f) * tan lat1)
    u2 = atan ((1 - f) * tan lat2)
    ll = long2 - long1
    la_good = convergeTo 0 1e-12 (iterate stepla ll)
    (fin_cos2al, fin_sinsig, fin_cossig, fin_cossigm2, _, fin_sig) = precalc la_good 
    up2 = fin_cos2al * (sq a - sq b) / sq b
    al = 1 + up2 / 16384 * (4096 + up2 * (-768 + up2 * (320 - 175 * up2)))
    bl = up2 / 1024 * (256 + up2 * (-128 + up2 * (74 - 47 * up2)))
    dsig = bl * fin_sinsig * (fin_cossigm2 + bl / 4 * (fin_cossig * (-1 + 2 * sq fin_cossigm2) - bl / 6 * fin_cossigm2 * (-3 + 4 * sq fin_sinsig * (-3 + 4 * sq fin_cossigm2))))
    azm = atan2 (cos u2 * sin la_good) (cos u1 * sin u2 - sin u1 * cos u2 * cos la_good)

    precalc :: Double -> (Double, Double, Double, Double, Double, Double)
    precalc la = (cos2al, sinsig, cossig, cossigm2, sinal, sig)
        where
        sinsig = sqrt (sq (cos u2 * sin la) + sq (cos u1 * sin u2 - sin u1 * cos u2 * cos la))
        cossig = sin u1 * sin u2 + cos u1 * cos u2 * cos la
        sig = atan2 sinsig cossig
        sinal = cos u1 * cos u2 * sin la / sinsig
        cos2al = 1 - sq sinal
        cossigm2 
            | cos2al == 0   = 0
            | otherwise     = cossig - 2 * sin u1 * sin u2 / cos2al

    stepla :: Double -> Double
    stepla la = ll + (1 - cl) * f * sinal * (sig + cl * sinsig * (cossigm2 + cl * cossig * (-1 + 2 * sq cossigm2)))
        where
        (cos2al, sinsig, cossig, cossigm2, sinal, sig) = precalc la 
        cl = f / 16 * cos2al * (4 + f * (4 - 3 * cos2al))

-- ---------------
-- azimuthDiff
-- | 'azimuthDiff' @first second@ calculates smallest angle between two azimuths.
azimuthDiff :: Double -> Double -> Double
azimuthDiff a1 a2
    | diff > pi     = 2 * pi - diff
    | diff < (-pi)  = 2 * pi + diff
    | otherwise     = diff
    where diff = a2 - a1
