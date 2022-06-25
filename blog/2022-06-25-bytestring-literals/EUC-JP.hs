#!/usr/bin/env stack
{- stack
  script
  --resolver lts-19.12
  --package bytestring
-}

module Main (main) where

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

------------------------------------------------------------------------------

-- 楽しいですね…
-- encoding: EUC-JP
message :: ByteString
message = BS.pack
    [ 0xB3, 0xDA, 0xA4, 0xB7, 0xA4, 0xA4, 0xA4, 0xC7
    , 0xA4, 0xB9, 0xA4, 0xCD, 0xA1, 0xC4, 0x0A
    ]

------------------------------------------------------------------------------

main :: IO ()
main = BS.putStr message
