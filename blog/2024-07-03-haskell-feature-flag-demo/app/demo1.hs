module Main where

-- (feature-flag-demo)
import qualified Demo1.FeatureFlag.DB
import qualified Demo1.IO

------------------------------------------------------------------------------

main :: IO ()
main = print =<< Demo1.IO.run =<< Demo1.FeatureFlag.DB.load
