{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

-- https://hackage.haskell.org/package/base
import Data.Typeable (Typeable)
import qualified System.IO as IO
import System.IO (Handle, IOMode(ReadMode))

-- https://hackage.haskell.org/package/explainable-predicates
import Test.Predicates (anything)

-- https://hackage.haskell.org/package/HMock
import Test.HMock ((|=>), expect, makeMockable, runMockT)

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (defaultMain)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), testCase)

------------------------------------------------------------------------------

class Monad m => MonadExp m where
  expShowInt :: Int -> m String
  expWithF :: Typeable r => FilePath -> (Int -> m r) -> m r

instance MonadExp IO where
  expShowInt = pure . show
  expWithF _ f = f 1

------------------------------------------------------------------------------

makeMockable [t|MonadExp|]

------------------------------------------------------------------------------

main :: IO ()
main = defaultMain . testCase "experiment" $ do
    result <- runMockT $ do
      expect $ ExpWithF_ anything
        |=> \(ExpWithF "one.txt" _action) -> pure "1"
      expWithF "one.txt" expShowInt
    "1" @=? result
