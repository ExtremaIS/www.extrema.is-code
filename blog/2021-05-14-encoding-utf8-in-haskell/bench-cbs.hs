module Main where

-- https://hackage.haskell.org/package/conduit
import qualified Conduit as C
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as CC

-- (bench-text:executable)
import Lib (benchmark, dataPath, sourceText)

------------------------------------------------------------------------------

main :: IO ()
main
    = benchmark
    $ C.runConduitRes
    $ CC.yieldMany sourceText
    .| CC.unlines
    .| CC.encodeUtf8
    .| CC.sinkFile dataPath
