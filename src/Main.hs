module Main where

import qualified Data.ByteString.Lazy as ByteString

import TCX

main::IO()
main = do
    xml <- ByteString.readFile "/home/blaze/Dropbox/Ski tracks/ski.tcx"
    let points = parseTCX xml
    print points
    print (length points)