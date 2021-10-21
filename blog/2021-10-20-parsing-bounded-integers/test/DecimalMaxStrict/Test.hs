{-# LANGUAGE OverloadedStrings #-}

module DecimalMaxStrict.Test where

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
import DecimalMaxStrict (word8)

------------------------------------------------------------------------------

parseWord8 :: Bool -> Text -> Either String Word8
parseWord8 isStrict = AT.parseOnly (word8 isStrict <* AT.endOfInput)

------------------------------------------------------------------------------

strictTests :: TestTree
strictTests = testGroup "Strict"
    [ testCase "empty" $
        Left "decimalMax: Failed reading: expected decimal"
          @=? parseWord8 True ""
    , testCase "zero" $ Right 0 @=? parseWord8 True "0"
    , testCase "max" $ Right 255 @=? parseWord8 True "255"
    , testCase "large1" $
        Left "decimalMax: Failed reading: overflow"
          @=? parseWord8 True "256"
    , testCase "large2" $
        Left "decimalMax: Failed reading: overflow"
          @=? parseWord8 True "1001"
    , testCase "paddedOK" $
        Left "decimalMax: Failed reading: leading zero"
          @=? parseWord8 True "00042"
    , testCase "paddedLarge" $
        Left "decimalMax: Failed reading: overflow"
          @=? parseWord8 True "01000"
    , testCase "negative" $
        Left "decimalMax: Failed reading: expected decimal"
          @=? parseWord8 True "-1"
    , testCase "imaginary" $ Left "endOfInput" @=? parseWord8 True "5i"
    ]

------------------------------------------------------------------------------

lenientTests :: TestTree
lenientTests = testGroup "Lenient"
    [ testCase "empty" $
        Left "decimalMax: Failed reading: expected decimal"
          @=? parseWord8 False ""
    , testCase "zero" $ Right 0 @=? parseWord8 False "0"
    , testCase "max" $ Right 255 @=? parseWord8 False "255"
    , testCase "large1" $
        Left "decimalMax: Failed reading: overflow"
          @=? parseWord8 False "256"
    , testCase "large2" $
        Left "decimalMax: Failed reading: overflow"
          @=? parseWord8 False "1001"
    , testCase "paddedOK" $ Right 42 @=? parseWord8 False "00042"
    , testCase "paddedLarge" $
        Left "decimalMax: Failed reading: overflow"
          @=? parseWord8 False "01000"
    , testCase "negative" $
        Left "decimalMax: Failed reading: expected decimal"
          @=? parseWord8 False "-1"
    , testCase "imaginary" $ Left "endOfInput" @=? parseWord8 False "5i"
    ]

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "DecimalMaxStrict"
    [ strictTests
    , lenientTests
    ]
