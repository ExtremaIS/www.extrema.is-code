------------------------------------------------------------------------------
-- |
-- Module      : Demo2.IO
-- Description : Demonstration in the IO monad
------------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}

module Demo2.IO
  ( -- * Public API
    run
    -- * Internal functions exported for testing
  , foo
  , bar
  ) where

-- (feature-flag-demo)
import qualified Demo2.FeatureFlag as FF

------------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------------

-- | Run the demo
--
-- The feature flag configuration is passed as a parameter so that the whole
-- program can be tested with various feature flag configurations (without
-- loading them from the database).
run :: FF.FeatureFlagMap -> IO Int
run ffMap = do
    FF.initialize ffMap
    fooBar

------------------------------------------------------------------------------
-- Internal functions exported for testing
------------------------------------------------------------------------------

-- | Pure function depending on a feature flag
--
-- To test pure functions, you can pass specific feature flag configuration
-- directly.  There is no need to use global storage.
foo :: FF.FeatureFlagConfig FF.Fix_202407_SomeBug_42 -> Int
foo ffSomeBug
    | FF.configT ffSomeBug = 42
    | otherwise            = 13

-- | Pure function depending on a feature flag
--
-- To test pure functions, you can pass specific feature flag configuration
-- directly.  There is no need to use global storage.
bar :: FF.FeatureFlagConfig FF.Ref_202407_SomeBusinessLogic -> Int -> Int
bar ffSomeBusinessLogic n
    | FF.configT ffSomeBusinessLogic = n + n
    | otherwise                      = 2 * n

------------------------------------------------------------------------------
-- Internal
------------------------------------------------------------------------------

-- | IO actions can lookup feature flags from the global storage
--
-- There is a compile-time error if you pass the wrong feature flag to a
-- function.
fooBar :: IO Int
fooBar = do
    ffSomeBug <- FF.lookupM FF.Fix_202407_SomeBug_42
    ffSomeBusinessLogic <- FF.lookupM FF.Ref_202407_SomeBusinessLogic
    pure . bar ffSomeBusinessLogic $ foo ffSomeBug
