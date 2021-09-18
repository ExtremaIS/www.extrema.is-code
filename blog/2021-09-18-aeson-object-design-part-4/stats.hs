#!/usr/bin/env stack
{- stack
  script
  --resolver lts-18.10
  --package bytestring
  --package cassava
  --package extra
  --package vector
-}

{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

-- https://hackage.haskell.org/package/base
import Data.Bifunctor (bimap)
import Data.List (find)
import Data.Maybe (fromMaybe, listToMaybe)

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString.Lazy as BSL

-- https://hackage.haskell.org/package/cassava
import qualified Data.Csv as CSV
import Data.Csv ((.!))

-- https://hackage.haskell.org/package/extra
import Data.List.Extra (groupOn)

-- https://hackage.haskell.org/package/vector
import qualified Data.Vector as Vector
import Data.Vector (Vector)

------------------------------------------------------------------------------

data InRow
  = InRow
    { inSize     :: !Int
    , inTypeName :: !String
    , inMeanSecs :: !Double
    }

instance CSV.FromRecord InRow where
  parseRecord v = do
    (inSize, inTypeName) <- bimap read (drop 1) . break (== '/') <$> v .! 0
    inMeanSecs <- read <$> v .! 1
    return InRow{..}

------------------------------------------------------------------------------

data OutRow
  = OutRow
    { outSize      :: !Int
    , outMeanSecsL :: !Double
    , outMeanSecsV :: !Double
    , outMeanSecsM :: !Double
    , outMeanSecsH :: !Double
    }

instance CSV.ToRecord OutRow where
  toRecord OutRow{..} = CSV.record
    [ CSV.toField outSize
    , CSV.toField outMeanSecsL
    , CSV.toField outMeanSecsV
    , CSV.toField outMeanSecsM
    , CSV.toField outMeanSecsH
    ]

------------------------------------------------------------------------------

go :: Double -> Vector InRow -> [OutRow]
go u = map mkOutRow . groupOn inSize . Vector.toList
  where
    mkOutRow :: [InRow] -> OutRow
    mkOutRow inRows = fromMaybe (error "oops") $ OutRow
      <$> (        inSize     <$> listToMaybe                        inRows)
      <*> ((* u) . inMeanSecs <$> find ((== "List")    . inTypeName) inRows)
      <*> ((* u) . inMeanSecs <$> find ((== "Vector")  . inTypeName) inRows)
      <*> ((* u) . inMeanSecs <$> find ((== "Map")     . inTypeName) inRows)
      <*> ((* u) . inMeanSecs <$> find ((== "HashMap") . inTypeName) inRows)

------------------------------------------------------------------------------

main :: IO ()
main
    = BSL.writeFile "stats.csv"
    . (CSV.encode [["Size", "List", "Vector", "Map", "HashMap"]] <>)
    . CSV.encode
    . go 1_000_000
    =<< either fail pure
    . CSV.decode CSV.HasHeader
    =<< BSL.readFile "report.csv"
