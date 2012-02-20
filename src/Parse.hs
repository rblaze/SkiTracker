module Parse (parseTrack) where

import Data.Maybe (fromJust)
import Text.XML.Light
import Text.XML.Light.Lexer (XmlSource)

import GPX
import TCX
import Track

parseTrack :: XmlSource t => t -> [TrackPoint]
parseTrack xml
    | name == "gpx"                     = parseGPX root
    | name == "TrainingCenterDatabase"  = parseTCX root
    | otherwise                         = error "unsupported data format"
    where
    root = fromJust $ parseXMLDoc xml
    name = qName $ elName root
