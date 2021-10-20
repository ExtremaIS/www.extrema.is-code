module Main (main) where

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (defaultMain, testGroup)

-- (parsing:test)
import qualified Decimal.Test
import qualified DecimalBounded.Test
import qualified DecimalBoundedStrict.Test
import qualified DecimalMaxFixed.Test
import qualified DecimalMaxSO.Test
import qualified DecimalMaxStrict.Test

------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "test"
    [ Decimal.Test.tests
    , DecimalBounded.Test.tests
    , DecimalBoundedStrict.Test.tests
    , DecimalMaxSO.Test.tests
    , DecimalMaxFixed.Test.tests
    , DecimalMaxStrict.Test.tests
    ]
