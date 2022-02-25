#!/usr/bin/env stack
{- stack
  script
  --resolver lts-18.25
  --package Color
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

-- https://hackage.haskell.org/package/base
import Data.Word (Word8)

-- https://hackage.haskell.org/package/Color
import Graphics.Color.Adaptation.VonKries (convert)
import Graphics.Color.Model (Color)
import Graphics.Color.Space.RGB.Alternative (HSV, Linearity(NonLinear))
import Graphics.Color.Space.RGB.SRGB (pattern ColorSRGB, SRGB)

------------------------------------------------------------------------------

blackSRGB :: Color (SRGB 'NonLinear) Word8
blackSRGB = ColorSRGB 0x00 0x00 0x00

blackHSV :: Color (HSV (SRGB 'NonLinear)) Float
blackHSV = convert blackSRGB

main :: IO ()
main = print blackHSV
