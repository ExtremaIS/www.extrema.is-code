{-# LANGUAGE OverloadedStrings #-}

module Main where

-- https://hackage.haskell.org/package/bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8

-- https://hackage.haskell.org/package/conduit
import qualified Conduit as C
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as CC

-- https://hackage.haskell.org/package/text
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

------------------------------------------------------------------------------

source :: ByteString
source = BS8.unlines ["first", "", "", "last"]

decodeUtf8LinesLenient
  :: Monad m
  => C.ConduitT ByteString Text m ()
decodeUtf8LinesLenient =
    C.awaitForever $ C.yield . TE.decodeUtf8With TEE.lenientDecode

main :: IO ()
main
    = mapM_ print
    . C.runConduitPure
    $ C.yield source
    .| CC.linesUnboundedAscii
    .| decodeUtf8LinesLenient
    .| CC.sinkList
