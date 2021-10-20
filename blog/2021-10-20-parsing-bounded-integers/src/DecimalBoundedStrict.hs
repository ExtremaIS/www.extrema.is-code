{-# LANGUAGE ScopedTypeVariables #-}

module DecimalBoundedStrict where

-- https://hackage.haskell.org/package/attoparsec
import qualified Data.Attoparsec.Text as AT
import Data.Attoparsec.Text ((<?>))

-- https://hackage.haskell.org/package/base
import Data.Char (isDigit, ord)

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T

------------------------------------------------------------------------------

strictDecimal :: forall a. Integral a => AT.Parser a
strictDecimal = (<?> "strictDecimal") $ do
    d <- AT.digit
    ds <- AT.takeWhile isDigit
    if d == '0' && not (T.null ds)
      then fail "leading zero"
      else return $ T.foldl' (\a c -> a * 10 + fromChar c) (fromChar d) ds
  where
    fromChar :: Char -> a
    fromChar c = fromIntegral $ ord c - 48
    {-# INLINE fromChar #-}

------------------------------------------------------------------------------

boundedDecimal :: forall a. (Bounded a, Integral a) => Bool -> AT.Parser a
boundedDecimal isStrict = (<?> "boundedDecimal") $ do
    i <- if isStrict then strictDecimal else AT.decimal <?> "decimal"
    if (i :: Integer) > fromIntegral (maxBound :: a)
      then fail "overflow"
      else return $ fromIntegral i
