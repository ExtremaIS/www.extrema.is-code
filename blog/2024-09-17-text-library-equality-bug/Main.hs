{-# LANGUAGE OverloadedStrings #-}

module Main where

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString.Lazy as BSL

-- https://hackage.haskell.org/package/text
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Internal.Lazy as TIL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

-- Invalid UTF-8
invalid :: BSL.ByteString
invalid = "test \xe3"

-- Expected decoded value when ignoring errors
expected :: TL.Text
expected = "test "

main :: IO ()
main = do
    let actual = TLE.decodeUtf8With TEE.ignore invalid
    putStrLn $ "Expected: " ++ TIL.showStructure expected
    putStrLn $ "Actual:   " ++ TIL.showStructure actual
    putStrLn $
      "Equality of lazy text:   " ++ show (expected == actual)
    putStrLn $
      "Equality of strict text: "
        ++ show (TL.toStrict expected == TL.toStrict actual)
