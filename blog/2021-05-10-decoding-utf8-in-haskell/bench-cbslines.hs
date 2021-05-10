module Main where

-- https://hackage.haskell.org/package/conduit
import qualified Conduit as C
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as CC

-- (bench-text:executable)
import Lib (benchmark, countFemale, dataPath)

------------------------------------------------------------------------------

main :: IO ()
main
    = benchmark
    $ (print =<<)
    $ C.runConduitRes
    $ CC.sourceFile dataPath
    .| CC.linesUnboundedAscii
    .| CC.decodeUtf8Lenient
    .| CC.foldl countFemale 0
