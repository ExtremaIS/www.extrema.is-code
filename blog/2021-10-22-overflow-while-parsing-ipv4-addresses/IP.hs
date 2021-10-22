#!/usr/bin/env stack
{- stack
  script
  --resolver lts-18.13
  --package ip
  --extra-dep bytebuild-0.3.8.0
  --extra-dep byteslice-0.2.6.0
  --extra-dep bytesmith-0.3.7.0
  --extra-dep contiguous-0.6.1
  --extra-dep natural-arithmetic-0.1.2.0
  --extra-dep primitive-offset-0.2.0.0
  --extra-dep run-st-0.1.1.0
  --extra-dep tuples-0.1.0.0
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

-- https://hackage.haskell.org/package/ip
import Net.IPv4 (decode)

------------------------------------------------------------------------------

main :: IO ()
main = print $ decode "127.0.0.18446744073709551617"
