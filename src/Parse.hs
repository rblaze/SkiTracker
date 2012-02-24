module Parse (parseTrack) where

import Data.Maybe (fromJust)
import Text.XML.Light
import Text.XML.Light.Lexer (XmlSource)

import GPX
import TCX
import Track

filterPoints :: [TrackPoint] -> [TrackPoint]
filterPoints [] = []
filterPoints [x] = [x]
filterPoints (x:xs) = x : filterPoints rest
    where
    rest = dropWhile sametime xs
    sametime point = tpTime x == tpTime point

parseTrack :: XmlSource t => t -> [TrackPoint]
parseTrack xml = filterPoints track
    where
    track
        | name == "gpx"                     = parseGPX root
        | name == "TrainingCenterDatabase"  = parseTCX root
        | otherwise                         = error "unsupported data format"
    root = fromJust $ parseXMLDoc xml
    name = qName $ elName root
