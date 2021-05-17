#!/usr/bin/env stack
{- stack script --resolver lts-17.11 -}

module Main where

payment
  :: (Int, Int, Int, Int, Int, Int)
  -> Int
  -> Maybe (Int, Int, Int, Int, Int, Int)
payment (c1, c5, c10, c50, c100, c500) due =
    let (c1', dueA) =
          case due `mod` 5 of
            n
              | n > 0 && n <= c1 -> (n, due - n)
              | otherwise        -> (0, due)
        (c5', dueB) =
          case dueA `mod` 10 of
            n
              | n > 0 && n <= 5 && c5 > 0 -> (1, dueA - 5)
              | otherwise                 -> (0, dueA)
        (c10', dueC) =
          case ceiling (fromIntegral dueB / 10 :: Float) `mod` 5 of
            n
              | n > 0 && n <= c10 -> (n, dueB - n * 10)
              | otherwise         -> (0, dueB)
        (c50', dueD) =
          case dueC `mod` 100 of
            n
              | n > 0 && n <= 50 && c50 > 0 -> (1, dueC - 50)
              | otherwise                   -> (0, dueC)
        (c100', dueE) =
          case ceiling (fromIntegral dueD / 100 :: Float) `mod` 5 of
            n
              | n > 0 && n <= c100 -> (n, dueD - n * 100)
              | otherwise          -> (0, dueD)
        (c500', dueF) =
          case dueE `mod` 1000 of
            n
              | n > 0 && n <= 500 && c500 > 0 -> (1, dueE - 500)
              | otherwise                     -> (0, dueE)
    in  if dueF <= 0
          then Just (c1', c5', c10', c50', c100', c500')
          else Nothing

main :: IO ()
main = print $ payment (2, 0, 4, 0, 2, 1) 276
