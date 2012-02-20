module GPX (parseGPX) where

import Control.Monad (join, liftM, liftM2, liftM3)
import Data.Maybe
import Data.Time (parseTime)
import System.Locale (defaultTimeLocale)
import Text.XML.Light

import Track

parseGpxPoint :: Element -> Maybe TrackPoint
parseGpxPoint x = liftM3 TrackPoint time pos alt
    where alt = parseChild read "ele"
          time = join $ parseChild (parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ") "time"
          pos = liftM2 Position lat lon

          lon = readAttrRad "lon"
          lat = readAttrRad "lat"

          getAttr name = findAttr blank_name { qName = name } x
          readAttrRad name = liftM (g2r . read) $ getAttr name

          g2r :: Double -> Double
          g2r angle = angle * pi / 180

          getChild :: String -> Maybe Element
          getChild name = findChild (elName x) { qName = name } x

          parseChild :: (String -> t) -> String -> Maybe t
          parseChild func attr = liftM (func . strContent) (getChild attr)


parseGPX :: Element -> [TrackPoint]
parseGPX root = mapMaybe parseGpxPoint $ findElements pointname root
    where
    pointname = (elName root) {qName = "trkpt"}
