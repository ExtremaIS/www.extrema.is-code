#!/usr/bin/env stack
{- stack
  script
  --resolver lts-18.13
  --package data-textual
  --package network-ip
-}

module Main where

-- https://hackage.haskell.org/package/data-textual
import Data.Textual (Parsed, parseString)

-- https://hackage.haskell.org/package/network-ip
import Network.IP.Addr (IP4)

------------------------------------------------------------------------------

main :: IO ()
main = print (parseString "127.0.0.18446744073709551617" :: Parsed IP4)
