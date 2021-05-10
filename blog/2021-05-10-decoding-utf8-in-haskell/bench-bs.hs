module Main where

-- https://hackage.haskell.org/package/base
import Data.List (foldl')

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString.Lazy as BSL

-- https://hackage.haskell.org/package/text
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

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
    . TLE.decodeUtf8With TEE.lenientDecode
    =<< BSL.readFile dataPath
