module Main where

-- (feature-flag-demo)
import qualified Demo2.FeatureFlag.DB
import qualified Demo2.IO

------------------------------------------------------------------------------

main :: IO ()
main = print =<< Demo2.IO.run =<< Demo2.FeatureFlag.DB.load
