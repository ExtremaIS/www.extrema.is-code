{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

-- https://hackage.haskell.org/package/base
import Control.Exception (evaluate)
import qualified Data.List as List

-- https://hackage.haskell.org/package/containers
import qualified Data.Map as Map

-- https://hackage.haskell.org/package/criterion
import qualified Criterion
import qualified Criterion.Main as CM
import qualified Criterion.Main.Options as CMO
import qualified Criterion.Types as CT

-- https://hackage.haskell.org/package/deepseq
import Control.DeepSeq (force)

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import Data.Text (Text)

-- https://hackage.haskell.org/package/unordered-containers
import qualified Data.HashMap.Strict as HashMap

-- https://hackage.haskell.org/package/vector
import qualified Data.Vector as Vector

------------------------------------------------------------------------------

goList :: [Text] -> [(Text, Int)] -> [Maybe Int]
goList keys pairs = [List.lookup key pairs | key <- keys]

------------------------------------------------------------------------------

goVector :: [Text] -> [(Text, Int)] -> [Maybe Int]
goVector keys pairs =
    let vec = Vector.fromList pairs
    in  [snd <$> Vector.find ((== key) . fst) vec | key <- keys]

------------------------------------------------------------------------------

goMap :: [Text] -> [(Text, Int)] -> [Maybe Int]
goMap keys pairs =
    let map' = Map.fromList pairs
    in  [Map.lookup key map' | key <- keys]

------------------------------------------------------------------------------

goHashMap :: [Text] -> [(Text, Int)] -> [Maybe Int]
goHashMap keys pairs =
    let map' = HashMap.fromList pairs
    in  [HashMap.lookup key map' | key <- keys]

------------------------------------------------------------------------------

bench :: Int -> IO Criterion.Benchmark
bench count = do
    keys  <- evaluate . force $ ("k" <>) . T.pack . show <$> [1 .. count]
    pairs <- evaluate . force $ (, 0) <$> keys
    return $ Criterion.bgroup (show count)
      [ Criterion.bench "List"    $ Criterion.nf (goList    keys) pairs
      , Criterion.bench "Vector"  $ Criterion.nf (goVector  keys) pairs
      , Criterion.bench "Map"     $ Criterion.nf (goMap     keys) pairs
      , Criterion.bench "HashMap" $ Criterion.nf (goHashMap keys) pairs
      ]

------------------------------------------------------------------------------

main :: IO ()
main = CM.defaultMainWith config =<< mapM bench [1..50]
  where
    config :: CT.Config
    config = CMO.defaultConfig
      { CT.reportFile = Just "report.html"
      , CT.csvFile    = Just "report.csv"
      }
