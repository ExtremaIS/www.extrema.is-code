{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module DecimalMaxSO.Test where

-- https://hackage.haskell.org/package/attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as ABS8

-- https://hackage.haskell.org/package/base
import Data.Word (Word8)

-- https://hackage.haskell.org/package/bytestring
import Data.ByteString (ByteString)

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), testCase)

-- (parsing)
import DecimalMaxSO (decimalMax)

------------------------------------------------------------------------------

parseWord8 :: ByteString -> Either String Word8
parseWord8 = ABS8.parseOnly $
    decimalMax (fromIntegral @Word8 maxBound) <* ABS8.endOfInput

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "DecimalMaxSO"
    [ testCase "empty" $
        Left "Failed reading: expected decimal" @=? parseWord8 ""
    , testCase "zero" $ Right 0 @=? parseWord8 "0"
    , testCase "max" $ Right 255 @=? parseWord8 "255"
    , testCase "large1" $
        Left "Failed reading: decimalMax: parsed decimal exceeded 255"
          @=? parseWord8 "256"
    , testCase "large1" $
        Left "Failed reading: decimalMax: parsed decimal exceeded 255"
          @=? parseWord8 "1001"
    , testCase "negative" $
        Left "Failed reading: expected decimal" @=? parseWord8 "-1"
    , testCase "imaginary" $ Left "endOfInput" @=? parseWord8 "5i"
    ]
