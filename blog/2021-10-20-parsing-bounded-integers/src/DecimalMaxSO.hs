{-# LANGUAGE TypeApplications #-}

module DecimalMaxSO where

-- https://hackage.haskell.org/package/attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as ABS8

-- https://hackage.haskell.org/package/base
import Data.Char (isDigit)
import Data.List (foldl')

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS

------------------------------------------------------------------------------

decimalMax :: Integral a => Integer -> ABS8.Parser a
decimalMax dMax = do
    let numDigs = ceiling @Float $ log (fromIntegral(dMax+1)) / log 10
        getVal = foldl' (\s d -> s*10+fromIntegral (d-48)) 0 . BS.unpack
    val <- getVal <$> ABS8.scan (0 :: Int) (\n c ->
            if n > numDigs || not (isDigit c) then Nothing else Just (n+1))
    if val <= dMax
      then return $ fromIntegral val
      else fail $ "decimalMax: parsed decimal exceeded " ++ show dMax
