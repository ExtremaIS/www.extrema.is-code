#!/usr/bin/env stack
{- stack
  script
  --resolver lts-18.13
  --package hw-ip
  --extra-dep generic-lens-2.2.0.0
  --extra-dep generic-lens-core-2.2.0.0
-}

{-# LANGUAGE TypeApplications #-}

module Main where

-- https://hackage.haskell.org/package/hw-ip
import HaskellWorks.Data.Network.Ip.Ipv4 (IpAddress)

------------------------------------------------------------------------------

main :: IO ()
main = print @IpAddress $ read "127.0.0.18446744073709551617"
