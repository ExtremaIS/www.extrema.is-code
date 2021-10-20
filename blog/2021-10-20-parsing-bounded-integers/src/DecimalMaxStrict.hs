{-# LANGUAGE TypeApplications #-}

module DecimalMaxStrict where

-- https://hackage.haskell.org/package/attoparsec
import qualified Data.Attoparsec.Text as AT
import Data.Attoparsec.Text ((<?>))

-- https://hackage.haskell.org/package/base
import Data.Char (isDigit, ord)
import Data.Word (Word8)

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T

------------------------------------------------------------------------------

decimalMax :: (Integral a, Integral b) => Bool -> a -> AT.Parser b
decimalMax isStrict valueMax = (<?> "decimalMax") $ do
    (t, mN) <- AT.runScanner (Just 0) $ \mN c -> case mN of
      Just n
        | isDigit c -> Just $
            let n' = n * 10 + fromIntegral (ord c - 48)
            in  if n' > valueMax then Nothing else Just n'
        | otherwise -> Nothing
      Nothing -> Nothing
    case (mN, T.uncons t) of
      (Just n, Just (d, ds))
        | isStrict && d == '0' && not (T.null ds) -> fail "leading zero"
        | otherwise -> return $ fromIntegral n
      (Just{}, Nothing) -> fail "expected decimal"
      (Nothing, _digits) -> fail "overflow"

------------------------------------------------------------------------------

word8 :: Bool -> AT.Parser Word8
word8 isStrict = decimalMax isStrict $ fromIntegral @Word8 @Int maxBound
