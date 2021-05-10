{-# LANGUAGE OverloadedStrings #-}

module Main where

-- https://hackage.haskell.org/package/bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8

-- https://hackage.haskell.org/package/conduit
import qualified Conduit as C
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as CC

------------------------------------------------------------------------------

source :: ByteString
source = BS8.unlines ["first", "", "", "last"]

main :: IO ()
main
    = mapM_ print
    . C.runConduitPure
    $ C.yield source
    .| CC.linesUnboundedAscii
    .| CC.decodeUtf8Lenient
    .| CC.sinkList
