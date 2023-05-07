{-# LANGUAGE TemplateHaskell #-}

module Main where

import Experiment (qpower)

------------------------------------------------------------------------------

qpowerFive :: Int -> Int
qpowerFive n = $$(qpower 5 [|| n ||])

------------------------------------------------------------------------------

main :: IO ()
main = print $ qpowerFive 2
