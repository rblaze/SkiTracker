module Parse (parseTrack) where

import Data.Maybe (fromJust)
import Data.List (groupBy)
import Data.Function (on)
import Text.XML.Light
import Text.XML.Light.Lexer (XmlSource)

import GPX
import TCX
import Track

filterPoints :: [TrackPoint] -> [TrackPoint]
filterPoints xs = map head $ groupBy ((==) `on` tpTime) xs

processTrack :: Maybe Element -> String -> [TrackPoint]
processTrack (Just root) "gpx" = parseGPX root
processTrack (Just root) "TrainingCenterDatabase" = parseTCX root
processTrack _ _ = error "unsupported data format"

parseTrack :: XmlSource t => t -> [TrackPoint]
parseTrack xml = filterPoints $ processTrack root name 
    where
    root = parseXMLDoc xml
    name = qName $ elName (fromJust root)
