module Main where

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString.Lazy as BSL

-- https://hackage.haskell.org/package/text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

-- (bench-text:executable)
import Lib (benchmark, dataPath, sourceText)

------------------------------------------------------------------------------

main :: IO ()
main
    = benchmark
    . BSL.writeFile dataPath
    . TLE.encodeUtf8
    . TL.unlines
    . map TL.fromStrict
    $ sourceText
