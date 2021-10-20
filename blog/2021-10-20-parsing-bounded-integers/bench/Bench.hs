module Main where

-- https://hackage.haskell.org/package/attoparsec
import qualified Data.Attoparsec.Text as AT

-- https://hackage.haskell.org/package/base
import Control.Exception (evaluate)
import Data.List (intercalate)
import Data.Word (Word8)
import Text.Printf (printf)

-- https://hackage.haskell.org/package/deepseq
import Control.DeepSeq (force)

-- https://hackage.haskell.org/package/gauge
import qualified Gauge

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import Data.Text (Text)

-- (parsing)
import qualified DecimalBoundedStrict
import qualified DecimalMaxStrict

------------------------------------------------------------------------------

goDecimalBoundedStrict :: Bool -> [Text] -> [Either String Word8]
goDecimalBoundedStrict isStrict = map . AT.parseOnly $
    DecimalBoundedStrict.boundedDecimal isStrict <* AT.endOfInput

------------------------------------------------------------------------------

goDecimalMaxStrict :: Bool -> [Text] -> [Either String Word8]
goDecimalMaxStrict isStrict = map . AT.parseOnly $
    DecimalMaxStrict.word8 isStrict <* AT.endOfInput

------------------------------------------------------------------------------

main :: IO ()
main = do
    validStrict  <- evaluate . force $ T.pack . show <$> validValues
    validLenient <- evaluate . force $ T.pack . printf "%03d" <$> validValues
    oflowStrict  <- evaluate . force $ T.pack . show <$> oflowValues
    oflowLenient <-
      evaluate . force $ T.pack . printf "%0102d" <$> oflowValues
    let dataOpts =
          [ (validStrict,  "validStrictData")
          , (validLenient, "validLenientData")
          , (oflowStrict,  "overflowStrictData")
          , (oflowLenient, "overflowLenientData")
          ]
        strictOpts =
          [ (True,  "strictParse")
          , (False, "lenientParse")
          ]
        implOpts =
          [ (goDecimalBoundedStrict, "DecimalBoundedStrict")
          , (goDecimalMaxStrict,     "DecimalMaxStrict")
          ]
    Gauge.defaultMain $ do
      (testData, dataLabel)   <- dataOpts
      (isStrict, strictLabel) <- strictOpts
      (go,       implLabel)   <- implOpts
      let label = intercalate ":" [implLabel, strictLabel, dataLabel]
      return . Gauge.bench label $ Gauge.nf (go isStrict) testData
  where
    validValues :: [Word8]
    validValues = [minBound .. maxBound]

    oflowBase :: Integer
    oflowBase = 10 ^ (100 :: Int)

    oflowValues :: [Integer]
    oflowValues = [oflowBase .. oflowBase + 1000]
