{-# LANGUAGE OverloadedStrings #-}

module DecimalBounded.Test where

-- https://hackage.haskell.org/package/attoparsec
import qualified Data.Attoparsec.Text as AT

-- https://hackage.haskell.org/package/base
import Data.Word (Word8)

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), testCase)

-- https://hackage.haskell.org/package/text
import Data.Text (Text)

-- (parsing)
import DecimalBounded (boundedDecimal)

------------------------------------------------------------------------------

parseWord8 :: Text -> Either String Word8
parseWord8 = AT.parseOnly (boundedDecimal <* AT.endOfInput)

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "DecimalBounded"
    [ testCase "empty" $ Left "not enough input" @=? parseWord8 ""
    , testCase "zero" $ Right 0 @=? parseWord8 "0"
    , testCase "max" $ Right 255 @=? parseWord8 "255"
    , testCase "large1" $ Left "Failed reading: overflow" @=? parseWord8 "256"
    , testCase "large2" $
        Left "Failed reading: overflow" @=? parseWord8 "1001"
    , testCase "paddedOK" $ Right 42 @=? parseWord8 "00042"
    , testCase "paddedLarge" $
        Left "Failed reading: overflow" @=? parseWord8 "01000"
    , testCase "negative" $
        Left "Failed reading: takeWhile1" @=? parseWord8 "-1"
    , testCase "imaginary" $ Left "endOfInput" @=? parseWord8 "5i"
    ]
