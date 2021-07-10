{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- https://hackage.haskell.org/package/binary
import qualified Data.Binary.Builder as BB

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString.Lazy.Char8 as BSL8

-- https://hackage.haskell.org/package/text
import Data.Text (Text)

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

------------------------------------------------------------------------------

newtype Username = Username Text

instance TTC.Render Username where
  render (Username t) = TTC.convert t

------------------------------------------------------------------------------

main :: IO ()
main = BSL8.putStrLn . BB.toLazyByteString . TTC.render $ Username "tcard"
