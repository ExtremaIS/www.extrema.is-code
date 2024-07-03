module Main (main) where

-- https://hackage.haskell.org/package/base
import System.Environment (setEnv)

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (defaultMain, testGroup)

-- (feature-flag-demo)
import qualified Demo1.IO.Test
import qualified Demo2.IO.Test

------------------------------------------------------------------------------

main :: IO ()
main = do
    setEnv "TASTY_NUM_THREADS" "1"  -- run only one test at a time!
    defaultMain $ testGroup "test"
      [ Demo1.IO.Test.tests
      , Demo2.IO.Test.tests
      ]
