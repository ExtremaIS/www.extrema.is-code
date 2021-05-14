{-# LANGUAGE OverloadedStrings #-}

module Lib where

-- https://hackage.haskell.org/package/base
import Data.Int (Int64)
import Data.Word (Word64)
import qualified GHC.Stats as Stats
import Text.Printf (printf)

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T

-- https://hackage.haskell.org/package/time
import Data.Time.LocalTime (diffLocalTime, getZonedTime, zonedTimeToLocalTime)

------------------------------------------------------------------------------

benchmark :: IO () -> IO ()
benchmark action = do
    time0 <- zonedTimeToLocalTime <$> getZonedTime
    action
    time1 <- zonedTimeToLocalTime <$> getZonedTime
    stats <- Stats.getRTSStats
    let maxMem  = Stats.max_live_bytes stats
        maxSlop = Stats.max_slop_bytes stats
        mutWall = nsToMs $ Stats.mutator_elapsed_ns stats
        gcWall  = nsToMs $ Stats.gc_elapsed_ns stats
        mutCpu  = nsToMs $ Stats.mutator_cpu_ns stats
        gcCpu   = nsToMs $ Stats.gc_cpu_ns stats
        prdWall = mutWall / (mutWall + gcWall)
        prdCpu  = mutCpu / (mutCpu + gcCpu)
    putStrLn $ "Wall clock time: " ++ show (time1 `diffLocalTime` time0)
    putStrLn $ "Maximum residency: " ++ showBytes maxMem
    putStrLn $ "Maximum slop: " ++ showBytes maxSlop
    putStrLn $ "Productivity (wall clock time): " ++ showPercent prdWall
    putStrLn $ "Productivity (CPU time): " ++ showPercent prdCpu
  where
    nsToMs :: Int64 -> Double
    nsToMs s = realToFrac s / 1000000.0

    showBytes :: Word64 -> String
    showBytes n
      | n >= 1000000000 =
          printf "%.1f GB" $ fromIntegral n / (1000000000.0 :: Float)
      | n >= 1000000 =
          printf "%.1f MB" $ fromIntegral n / (1000000.0 :: Float)
      | n >= 1000 =
          printf "%.1f KB" $ fromIntegral n / (1000.0 :: Float)
      | otherwise = show n ++ " B"

    showPercent :: Double -> String
    showPercent = printf "%.1f %%" . (* 100)

------------------------------------------------------------------------------

dataPath :: FilePath
dataPath = "output.txt"

------------------------------------------------------------------------------

sourceText :: [T.Text]
sourceText =
    T.pack . show <$> enumFromTo (maxBound - 10000000) (maxBound :: Word64)
