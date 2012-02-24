module TCX (parseTCX) where

import Control.Monad (join, liftM, liftM2, liftM3)
import Data.Maybe
import Data.Time (parseTime)
import System.Locale (defaultTimeLocale)
import Text.XML.Light

import Track

parseTcxPoint :: Element -> Maybe TrackPoint
parseTcxPoint x = liftM3 TrackPoint time pos alt
    where alt = readAttr "AltitudeMeters" x
          time = join $ parseAttr (parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ") "Time" x
          pos = join $ liftM parsePosition $ getChild "Position" x  

          parsePosition :: Element -> Maybe Position
          parsePosition el = liftM2 Position (liftM g2r $ readAttr "LatitudeDegrees" el) (liftM g2r $ readAttr "LongitudeDegrees" el)

          g2r :: Double -> Double
          g2r angle = angle * pi / 180
          
          getChild :: String -> Element -> Maybe Element
          getChild name = findChild (elName x) { qName = name }

          parseAttr :: (String -> t) -> String -> Element -> Maybe t
          parseAttr func attr el = liftM (func . strContent) (getChild attr el)
          
          readAttr :: Read t => String -> Element -> Maybe t
          readAttr = parseAttr read

parseTCX :: Element -> [TrackPoint]
parseTCX root = mapMaybe parseTcxPoint $ findElements pointname root
    where
    pointname = (elName root) {qName = "Trackpoint"}
