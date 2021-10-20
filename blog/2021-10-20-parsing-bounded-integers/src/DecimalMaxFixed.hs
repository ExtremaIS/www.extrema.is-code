{-# LANGUAGE TypeApplications #-}

module DecimalMaxFixed where

-- https://hackage.haskell.org/package/attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as ABS8
import Data.Attoparsec.ByteString.Char8 ((<?>))

-- https://hackage.haskell.org/package/base
import Control.Monad (when)
import Data.Char (isDigit)

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS

------------------------------------------------------------------------------

decimalMax :: Integral a => Integer -> ABS8.Parser a
decimalMax valueMax = (<?> "decimalMax") $ do
    bs <- ABS8.scan 0 $ \numDigits c ->
      if numDigits <= numDigitsMax && isDigit c
        then Just $ numDigits + 1
        else Nothing
    when (BS.null bs) $ fail "expected decimal"
    let value = BS.foldl' (\acc d -> acc * 10 + fromIntegral (d - 48)) 0 bs
    if value > valueMax
      then fail $ "decimal exceeds " ++ show valueMax
      else return $ fromIntegral value
  where
    numDigitsMax :: Int
    numDigitsMax = ceiling @Float $ logBase 10 (fromIntegral valueMax + 1)
