{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Typeable (Typeable)

-- https://hackage.haskell.org/package/explainable-predicates
import Test.Predicates (anything)

-- https://hackage.haskell.org/package/HMock
import qualified Test.HMock as HMock
import Test.HMock ((|=>))

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (defaultMain)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit (assertBool, testCase)

-- https://hackage.haskell.org/package/MonadRandom
import qualified Control.Monad.Random.Strict as Rand

------------------------------------------------------------------------------

class Monad m => MonadTypeableRandom m where
  getRandomR :: (Rand.Random a, Typeable a) => (a, a) -> m a

  default getRandomR
    :: (Rand.MonadRandom m, Rand.Random a, Typeable a)
    => (a, a) -> m a
  getRandomR = Rand.getRandomR
  {-# INLINE getRandomR #-}

  getRandom :: (Rand.Random a, Typeable a) => m a

  default getRandom
    :: (Rand.MonadRandom m, Rand.Random a, Typeable a)
    => m a
  getRandom = Rand.getRandom
  {-# INLINE getRandom #-}

  getRandomRs :: (Rand.Random a, Typeable a) => (a, a) -> m [a]

  default getRandomRs
    :: (Rand.MonadRandom m, Rand.Random a, Typeable a)
    => (a, a) -> m [a]
  getRandomRs = Rand.getRandomRs
  {-# INLINE getRandomRs #-}

  getRandoms :: (Rand.Random a, Typeable a) => m [a]

  default getRandoms
    :: (Rand.MonadRandom m, Rand.Random a, Typeable a)
    => m [a]
  getRandoms = Rand.getRandoms
  {-# INLINE getRandoms #-}

------------------------------------------------------------------------------

HMock.makeMockable [t|MonadTypeableRandom|]

------------------------------------------------------------------------------

example :: MonadTypeableRandom m => m Bool
example = (== 0) <$> getRandomR (0, 1 :: Int)

------------------------------------------------------------------------------

main :: IO ()
main = defaultMain . testCase "experiment" . HMock.runMockT $ do
    HMock.expect $ GetRandomR_ anything |=>
      \(GetRandomR (0, 1 :: Int)) -> pure 0
    liftIO . assertBool "True" =<< example
