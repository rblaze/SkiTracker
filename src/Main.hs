module Main where

import qualified Data.ByteString.Lazy as ByteString

import TCX
import Track

main::IO()
main = do
    xml <- ByteString.readFile "/home/blaze/Dropbox/Ski tracks/ski.tcx"
    let track = parseTCX xml
    print (trackLength track)