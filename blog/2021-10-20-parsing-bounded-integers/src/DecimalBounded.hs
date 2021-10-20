{-# LANGUAGE ScopedTypeVariables #-}

module DecimalBounded where

-- https://hackage.haskell.org/package/attoparsec
import qualified Data.Attoparsec.Text as AT

------------------------------------------------------------------------------

boundedDecimal :: forall a. (Bounded a, Integral a) => AT.Parser a
boundedDecimal = do
    i <- AT.decimal
    if (i :: Integer) > fromIntegral (maxBound :: a)
      then fail "overflow"
      else return $ fromIntegral i
