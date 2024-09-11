#!/usr/bin/env cabal
{- cabal:
build-depends: base, bytestring, text
-}
{- project:
with-compiler: ghc-9.6.6
-}

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding.Error as TEE
import Data.Text.Lazy ()
import qualified Data.Text.Lazy.Encoding as TLE

good, bad :: BSL.ByteString
good = "\0g\0o\0o\0d"
bad  = "\0b\0a\0d\0"

main :: IO ()
main = do
    print good
    print $ TLE.decodeUtf16BEWith TEE.lenientDecode good
    print bad
    print $ TLE.decodeUtf16BEWith TEE.lenientDecode bad
