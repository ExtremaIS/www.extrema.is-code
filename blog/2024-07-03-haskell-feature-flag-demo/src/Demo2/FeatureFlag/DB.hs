------------------------------------------------------------------------------
-- |
-- Module      : Demo2.FeatureFlag.DB
-- Description : Mocked feature flag configuration loading
------------------------------------------------------------------------------

module Demo2.FeatureFlag.DB (load) where

-- https://hackage.haskell.org/package/dependent-map
import qualified Data.Dependent.Map as DMap

-- https://hackage.haskell.org/package/dependent-sum
import Data.Dependent.Sum ((==>))

-- (feature-flag-demo)
import qualified Demo2.FeatureFlag as FF

------------------------------------------------------------------------------

-- | Load feature flag configuration from the database
--
-- This is mocked for the demo.
load :: IO FF.FeatureFlagMap
load = pure $ DMap.fromList
    [ FF.Fix_202407_SomeBug_42 ==> True
    , FF.Ref_202407_SomeBusinessLogic ==> True
    ]
