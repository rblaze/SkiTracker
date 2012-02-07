module Main where

import Text.XML.Light
import qualified Data.ByteString.Lazy as ByteString
import Data.Maybe
import Data.Time
import System.Locale

data Position = Position Double Double  -- Lat Long
    deriving (Show)
data TrackPoint = TrackPoint UTCTime Position Double -- Altitude
    deriving (Show)

getQualData :: String -> Element -> Maybe String
getQualData s x = strContent `fmap` findChild (elName x) { qName = s } x 

parsePoint :: Element -> Maybe TrackPoint
parsePoint x = makePoint ts pos alt
    where makePoint :: Maybe UTCTime -> Maybe Position -> Maybe Double -> Maybe TrackPoint
          makePoint (Just a) (Just b) (Just c) = Just $ TrackPoint a b c
          makePoint _ _ _ = Nothing

          readAttr n e = read `fmap` getQualData n e

          alt = readAttr "AltitudeMeters" x

          timestr = getQualData "Time" x
          ts = maybe Nothing (parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ") timestr

          posdata = findChild (elName x) { qName = "Position" } x
          pos = if isJust posdata 
            then parsePos (readAttr "LatitudeDegrees" $ fromJust posdata) (readAttr "LongitudeDegrees" $ fromJust posdata)
            else Nothing
          parsePos (Just a) (Just b) = Just $ Position a b
          parsePos _ _ = Nothing

main::IO()
main = do
    xml <- ByteString.readFile "/home/blaze/Dropbox/Ski tracks/ski.tcx"
    let root = fromJust $ parseXMLDoc xml
    let tname = elName root
    let points = mapMaybe parsePoint $ findElements tname {qName = "Trackpoint"} root
    print points
    print (length points)