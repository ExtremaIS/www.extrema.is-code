------------------------------------------------------------------------------
-- |
-- Module      : Demo1.FeatureFlag.DB
-- Description : Mocked feature flag configuration loading
------------------------------------------------------------------------------

module Demo1.FeatureFlag.DB (load) where

-- https://hackage.haskell.org/package/containers
import qualified Data.Map.Strict as Map

-- (feature-flag-demo)
import qualified Demo1.FeatureFlag as FF

------------------------------------------------------------------------------

-- | Load feature flag configuration from the database
--
-- This is mocked for the demo.
load :: IO FF.FeatureFlagMap
load = pure $ Map.fromList
    [ (FF.Fix_202407_SomeBug_42, True)
    , (FF.Ref_202407_SomeBusinessLogic, True)
    ]
