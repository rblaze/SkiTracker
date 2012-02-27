
module Maps(getMap, encodeNumber) where

import qualified Data.ByteString.Lazy as BS
import Network.Curl.Download.Lazy
import Control.Monad
import Data.Bits
import Data.Char
import Data.Either()
import Text.Printf

import Track

import Debug.Trace

encodeNumber :: Double -> String
encodeNumber num = map (\x -> chr (x + 63)) values
    where
    mult = round (num * 100000) `shiftL` 1
    chunks = if num > 0 then mkchunks mult else mkchunks (complement mult)
    values = map (.|. 0x20) (init chunks) ++ [last chunks]
    mkchunks 0 = []
    mkchunks x = (x .&. 0x1F) : mkchunks (x `shiftR` 5)

encodeCoord :: Double -> String
encodeCoord x = encodeNumber (x / pi * 180.0)

makeRequest :: [[TrackPoint]] -> String
makeRequest points = trace (show (length url) ++ " " ++ url) url
    where
    baseurl = uncurry (printf "http://maps.googleapis.com/maps/api/staticmap?size=%dx%d&maptype=terrain&sensor=false") mapsize
    url = baseurl ++ concatMap mkline points
    mapsize :: (Int, Int)
    mapsize = (640, 640)
    mkline lift = printf "&path=enc:%s" coord 
        where
        Position sx sy = tpPos (head lift)
        Position ex ey = tpPos (last lift)
        coord = concatMap encodeCoord [sx, sy, ex - sx, ey - sy]

getMap :: [[TrackPoint]] -> IO BS.ByteString
getMap lifts = liftM getData stream
    where
    getData (Right x) = x
    getData (Left x) = error ("Can't download map: " ++ x) 
    stream = openLazyURI (makeRequest lifts)
