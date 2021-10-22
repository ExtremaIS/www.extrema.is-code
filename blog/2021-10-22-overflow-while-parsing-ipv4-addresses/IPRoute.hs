#!/usr/bin/env stack
{- stack
  script
  --resolver lts-18.13
  --package iproute
-}

{-# LANGUAGE TypeApplications #-}

module Main where

-- https://hackage.haskell.org/package/iproute
import Data.IP (IPv4)

------------------------------------------------------------------------------

main :: IO ()
main = print @IPv4 $ read "127.0.0.18446744073709551617"
