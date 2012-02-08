module TCX (parseTCX) where

import Control.Monad (join, liftM, liftM2, liftM3)
import Data.Maybe
import Data.Time (parseTime)
import System.Locale (defaultTimeLocale)
import Text.XML.Light
import Text.XML.Light.Lexer (XmlSource)

import Track

parseTcxPoint :: Element -> Maybe TrackPoint
parseTcxPoint x = liftM3 TrackPoint time pos alt
    where alt = readAttr "AltitudeMeters" x
          time = join $ parseAttr (parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ") "Time" x
          pos = join $ liftM parsePosition $ getChild "Position" x  

          parsePosition :: Element -> Maybe Position
          parsePosition el = liftM2 Position (readAttr "LatitudeDegrees" el) (readAttr "LongitudeDegrees" el)

          getChild :: String -> Element -> Maybe Element
          getChild name = findChild (elName x) { qName = name }

          parseAttr :: (String -> t) -> String -> Element -> Maybe t
          parseAttr func attr el = liftM (func . strContent) (getChild attr el)
          
          readAttr :: Read t => String -> Element -> Maybe t
          readAttr = parseAttr read

parseTCX :: XmlSource t => t -> [TrackPoint]
parseTCX xml = mapMaybe parseTcxPoint $ findElements pointname root
    where root = fromJust $ parseXMLDoc xml
          pointname = (elName root) {qName = "Trackpoint"}
