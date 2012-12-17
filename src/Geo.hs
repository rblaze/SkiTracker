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

calcΔσ :: Double -> Double -> Double -> Double -> Double
calcΔσ _B sinσ cosσ cos_σm2 = _B * sinσ * (
        cos_σm2 + _B / 4 * (
            cosσ * (-1 + 2 * sq cos_σm2)
            - _B / 6 * cos_σm2 * (-3 + 4 * sq sinσ * (-3 + 4 * sq cos_σm2))
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
    σ1 = atan2 tanU1 (cos azimuth)
    sinα = cosU1 * sin azimuth
    cos_sqα = (1 - sinα) * (1 + sinα)
    u2 = calcU2 cos_sqα
    _A = calcA u2
    _B = calcB u2
    initsigma = dist / (b * _A)
    directStep σ' = let σm2 = 2 * σ1 + σ'
                        _Δσ = calcΔσ _B (sin σ') (cos σ') (cos σm2)
                     in dist / (b * _A) + _Δσ
    σ = convergeTo 0 1e-12 (iterate directStep initsigma)
    l2h = sinU1 * cos σ + cosU1 * sin σ * cos azimuth
    l2l = (1 - f) * sqrt (sq sinα + sq (sinU1 * sin σ - cosU1 * cos σ * cos azimuth))
    lat2 = atan2 l2h l2l

    λ = atan2 (sin σ * sin azimuth) (cosU1 * cos σ - sinU1 * sin σ * cos azimuth)
    _C = f / 16 * cos_sqα * (4 + f * (4 - 3 * cos_sqα))
    _L = let σm2 = 2 * σ1 + σ
             in λ - (1 - _C) * f * sinα * (
                    σ + _C * sin σ * (
                        cos σm2 + _C * cos σ * ((-1) + 2 * sq (cos σm2))
                      )
                  )

    long2 = angleSum long _L
    angleSum a1 a2
        | suma > pi     = suma - 2 * pi
        | suma < (-pi)  = suma + 2 * pi
        | otherwise     = suma
        where suma = a1 + a2

inverseCalc :: Double -> Double -> Double -> (Double, Double, Double, Double, Double, Double)
inverseCalc λ _U1 _U2 = (cos_sqα, sinσ, cosσ, cosσm2, sinα, σ)
    where
    sinσ = sqrt (sq (cos _U2 * sin λ) + sq (cos _U1 * sin _U2 - sin _U1 * cos _U2 * cos λ))
    cosσ = sin _U1 * sin _U2 + cos _U1 * cos _U2 * cos λ
    σ = atan2 sinσ cosσ
    sinα = cos _U1 * cos _U2 * sin λ / sinσ
    cos_sqα = 1 - sq sinα
    cosσm2 
        | cos_sqα == 0  = 0
        | otherwise     = cosσ - 2 * sin _U1 * sin _U2 / cos_sqα

inverseStep :: Double -> Double -> Double -> Double -> Double
inverseStep _U1 _U2 _L λ = _L + (1 - _C) * f * sinα * (
                                sig + _C * sinσ * (
                                    cosσm2 + _C * cosσ * (-1 + 2 * sq cosσm2)
                                  )
                              )
    where
    (cos2α, sinσ, cosσ, cosσm2, sinα, sig) = inverseCalc λ _U1 _U2
    _C = f / 16 * cos2α * (4 + f * (4 - 3 * cos2α))

-- | 'vincentyFormulae' @start end@ calculates distance and azimuth
-- from @start@ to @end@ on Earth's surface.
vincentyFormulae :: Position -> Position -> DistVector
vincentyFormulae (Position lat1 long1) (Position lat2 long2) 
    | lat1 == lat2 && long1 == long2    = DistVector 0 0 
    | otherwise                         = DistVector {dvDistance = round2mm (b * _A * (σ - _Δσ)), dvAzimuth = azm}
    where
    _U1 = atan ((1 - f) * tan lat1)
    _U2 = atan ((1 - f) * tan lat2)
    _L = long2 - long1
    λ = convergeTo 0 1e-12 $ iterate (inverseStep _U1 _U2 _L) _L
    (cos_sqα, sinσ, cosσ, cosσm2, _, σ) = inverseCalc λ _U1 _U2
    up2 = calcU2 cos_sqα
    _A = calcA up2
    _B = calcB up2
    _Δσ = calcΔσ _B sinσ cosσ cosσm2
    azm = atan2 (cos _U2 * sin λ) (cos _U1 * sin _U2 - sin _U1 * cos _U2 * cos λ)


-- | 'azimuthDiff' @first second@ calculates smallest angle between two azimuths.
azimuthDiff :: Double -> Double -> Double
azimuthDiff a1 a2
    | diff > pi     = 2 * pi - diff
    | diff < (-pi)  = 2 * pi + diff
    | otherwise     = diff
    where diff = a2 - a1
