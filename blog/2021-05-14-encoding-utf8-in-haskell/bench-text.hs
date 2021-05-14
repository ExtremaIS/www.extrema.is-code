module Main where

-- https://hackage.haskell.org/package/text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

-- (bench-text:executable)
import Lib (benchmark, dataPath, sourceText)

------------------------------------------------------------------------------

main :: IO ()
main
    = benchmark
    . TLIO.writeFile dataPath
    . TL.unlines
    . map TL.fromStrict
    $ sourceText
