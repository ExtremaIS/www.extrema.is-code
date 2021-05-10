module Main where

-- https://hackage.haskell.org/package/base
import Data.List (foldl')

-- https://hackage.haskell.org/package/text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

-- (bench-text:executable)
import Lib (benchmark, countFemale, dataPath)

------------------------------------------------------------------------------

main :: IO ()
main
    = benchmark
    $ print
    . foldl' countFemale 0
    . map TL.toStrict
    . TL.lines
    =<< TLIO.readFile dataPath
