module Main where

-- https://hackage.haskell.org/package/bytestring
import Data.ByteString (ByteString)

-- https://hackage.haskell.org/package/conduit
import qualified Conduit as C
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as CC

-- https://hackage.haskell.org/package/text
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

-- (bench-text:executable)
import Lib (benchmark, countFemale, dataPath)

------------------------------------------------------------------------------

decodeUtf8LinesLenient
  :: Monad m
  => C.ConduitT ByteString Text m ()
decodeUtf8LinesLenient =
    C.awaitForever $ C.yield . TE.decodeUtf8With TEE.lenientDecode

main :: IO ()
main
    = benchmark
    $ (print =<<)
    $ C.runConduitRes
    $ CC.sourceFile dataPath
    .| CC.linesUnboundedAscii
    .| decodeUtf8LinesLenient
    .| CC.foldl countFemale 0
