module Geo(Position(..), DistVector(..),
    vincentyFormulae, vincentyFormulae', azimuthDiff) where

import Math.Sequence.Converge (convergeTo)

import Util

data Position = Position Double Double  -- Lat Long
    deriving (Show, Eq)
data DistVector = DistVector { dvDistance :: Double, dvAzimuth :: Double }
    deriving (Show)

-- | length of major axis of the ellipsoid (radius at equator)
a :: Double
a = 6378137.0

-- | flattening of the ellipsoid
f :: Double
f = 1/298.257223563

-- | length of minor axis of the ellipsoid (radius at the poles)
b :: Double
b = (1 - f) * a

calcA :: Double -> Double
calcA x = 1 + x / 16384 * (4096 + x * (-768 + x * (320 - 175 * x)))

calcB :: Double -> Double
calcB x = x / 1024 * (256 + x * (-128 + x * (74 - 47 * x)))

calcU2 :: Double -> Double
calcU2 x = x * (sq a - sq b) / sq b

calcDSig :: Double -> Double -> Double -> Double -> Double
calcDSig _B sinsig cossig cos2sigm = _B * sinsig * (
        cos2sigm + _B / 4 * (
            cossig * (-1 + 2 * sq cos2sigm)
            - _B / 6 * cos2sigm * (-3 + 4 * sq sinsig * (-3 + 4 * sq cos2sigm))
          )
      )

-- | Given an initial point, and initial azimuth, and a distance, along the geodesic
-- the problem is to find the end point
vincentyFormulae' :: Position -> DistVector -> Position
vincentyFormulae' (Position lat long) (DistVector dist azimuth)
    | dist == 0     = Position lat long
    | otherwise     = Position lat2 long2
    where
    tanU1 = (1 - f) * tan lat
    sinU1 = tanU1 / sqrt (1 + sq tanU1)
    cosU1 = 1 / sqrt (1 + sq tanU1)
    sig1 = atan2 tanU1 (cos azimuth)
    sina = cosU1 * sin azimuth
    cos2a = (1 - sina) * (1 + sina)
    u2 = calcU2 cos2a
    _A = calcA u2
    _B = calcB u2
    initsigma = dist / (b * _A)
    directStep s = let sigm2 = 2 * sig1 + s
                       dsig = calcDSig _B (sin s) (cos s) (cos sigm2)
                    in dist / (b * _A) + dsig
    sig = convergeTo 0 1e-12 (iterate directStep initsigma)
    l2h = sinU1 * cos sig + cosU1 * sin sig * cos azimuth
    l2l = (1 - f) * sqrt (sq sina + sq (sinU1 * sin sig - cosU1 * cos sig * cos azimuth))
    lat2 = atan2 l2h l2l

    lambda = atan2 (sin sig * sin azimuth) (cosU1 * cos sig - sinU1 * sin sig * cos azimuth)
    _C = f / 16 * cos2a * (4 + f * (4 - 3 * cos2a))
    _L = let sigm2 = 2 * sig1 + sig
             in lambda - (1 - _C) * f * sina * (
                    sig + _C * sin sig * (
                        cos sigm2 + _C * cos sig * ((-1) + 2 * sq (cos sigm2))
                      )
                  )

    long2 = angleSum long _L
    angleSum a1 a2
        | suma > pi     = suma - 2 * pi
        | suma < (-pi)  = suma + 2 * pi
        | otherwise     = suma
        where suma = a1 + a2

-- | 'vincentyFormulae' @start end@ calculates distance and azimuth
-- from @start@ to @end@ on Earth's surface.
vincentyFormulae :: Position -> Position -> DistVector
vincentyFormulae (Position lat1 long1) (Position lat2 long2) 
    | lat1 == lat2 && long1 == long2    = DistVector 0 0 
    | otherwise                         = DistVector {dvDistance = round2mm (b * _A * (fin_sig - dsig)), dvAzimuth = azm}
    where
    _U1 = atan ((1 - f) * tan lat1)
    _U2 = atan ((1 - f) * tan lat2)
    _L = long2 - long1
    lambda = convergeTo 0 1e-12 (iterate inverseStep _L)
    (fin_cos2al, fin_sinsig, fin_cossig, fin_cossigm2, _, fin_sig) = precalc lambda 
    up2 = calcU2 fin_cos2al
    _A = calcA up2
    _B = calcB up2
    dsig = calcDSig _B fin_sinsig fin_cossig fin_cossigm2
    azm = atan2 (cos _U2 * sin lambda) (cos _U1 * sin _U2 - sin _U1 * cos _U2 * cos lambda)

    precalc :: Double -> (Double, Double, Double, Double, Double, Double)
    precalc la = (cos2al, sinsig, cossig, cossigm2, sinal, sig)
        where
        sinsig = sqrt (sq (cos _U2 * sin la) + sq (cos _U1 * sin _U2 - sin _U1 * cos _U2 * cos la))
        cossig = sin _U1 * sin _U2 + cos _U1 * cos _U2 * cos la
        sig = atan2 sinsig cossig
        sinal = cos _U1 * cos _U2 * sin la / sinsig
        cos2al = 1 - sq sinal
        cossigm2 
            | cos2al == 0   = 0
            | otherwise     = cossig - 2 * sin _U1 * sin _U2 / cos2al

    inverseStep :: Double -> Double
    inverseStep la = let (cos2al, sinsig, cossig, cossigm2, sinal, sig) = precalc la
                         _C = f / 16 * cos2al * (4 + f * (4 - 3 * cos2al))
                      in _L + (1 - _C) * f * sinal * (
                            sig + _C * sinsig * (
                                cossigm2 + _C * cossig * (-1 + 2 * sq cossigm2)
                              )
                          )

-- | 'azimuthDiff' @first second@ calculates smallest angle between two azimuths.
azimuthDiff :: Double -> Double -> Double
azimuthDiff a1 a2
    | diff > pi     = 2 * pi - diff
    | diff < (-pi)  = 2 * pi + diff
    | otherwise     = diff
    where diff = a2 - a1
