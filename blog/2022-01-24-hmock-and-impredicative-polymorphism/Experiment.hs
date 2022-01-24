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

-- | Handle I/O
class Monad m => MonadHandle m where
  hIsEOF :: Handle -> m Bool
  withFile :: Typeable r => FilePath -> IOMode -> (Handle -> m r) -> m r

instance MonadHandle IO where
  hIsEOF = IO.hIsEOF
  withFile = IO.withFile

------------------------------------------------------------------------------

makeMockable [t|MonadHandle|]

------------------------------------------------------------------------------

isFileEmpty :: MonadHandle m => FilePath -> m Bool
isFileEmpty path = withFile path ReadMode hIsEOF

------------------------------------------------------------------------------

main :: IO ()
main = defaultMain . testCase "experiment" $ do
    isEmpty <- runMockT $ do
      expect $ WithFile_ anything
        |=> \(WithFile "test.txt" ReadMode _action) -> pure True
      isFileEmpty "test.txt"
    True @=? isEmpty
