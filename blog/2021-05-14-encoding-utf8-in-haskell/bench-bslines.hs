module Main where

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8

-- https://hackage.haskell.org/package/text
import qualified Data.Text.Encoding as TE

-- (bench-text:executable)
import Lib (benchmark, dataPath, sourceText)

------------------------------------------------------------------------------

main :: IO ()
main
    = benchmark
    . BSL.writeFile dataPath
    . BSL8.unlines
    . map (BSL.fromStrict . TE.encodeUtf8)
    $ sourceText
