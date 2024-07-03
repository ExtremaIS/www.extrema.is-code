------------------------------------------------------------------------------
-- |
-- Module      : Demo2.FeatureFlag
-- Description : Feature flags with tagged configuration
------------------------------------------------------------------------------

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo2.FeatureFlag
  ( -- * Current Feature Flags
    FeatureFlag(..)
    -- * Feature Flag Configuration
  , FeatureFlagMap
  , FeatureFlagConfig(..)
  , configT
  , configF
  , lookup
    -- * Global Storage
  , initialize
    -- * Usage API
  , MonadFeatureFlags(..)
  , lookupM
  , HasFeatureFlags(..)
  ) where

-- https://hackage.haskell.org/package/base
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent.MVar (MVar)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor.Identity (Identity(runIdentity))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)
import System.IO.Unsafe (unsafePerformIO)

-- https://hackage.haskell.org/package/dependent-map
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Map (DMap)

-- https://hackage.haskell.org/package/dependent-sum-template
import Data.GADT.Compare.TH
  ( DeriveGCompare(deriveGCompare), DeriveGEQ(deriveGEq)
  )
import Data.GADT.Show.TH (DeriveGShow(deriveGShow))

-- https://hackage.haskell.org/package/transformers
import Control.Monad.Trans.Reader (ReaderT, asks)

------------------------------------------------------------------------------
-- Current Feature Flags
------------------------------------------------------------------------------

-- | Currently-supported feature flags
--
-- Note that the constructor names purposefully do not follow usual Haskell
-- conventions.  In this demo, there are two types of feature flags:
--
-- * Bug fixes have a @Fix@ prefix.
-- * Refactors have a @Ref@ prefix.
--
-- The constructors encode the month, a concise description, and (optionally)
-- an issue/ticket number.
data FeatureFlag :: Type -> Type where
  Fix_202407_SomeBug_42 :: FeatureFlag Bool
  Ref_202407_SomeBusinessLogic :: FeatureFlag Bool

deriveGEq ''FeatureFlag
deriveGCompare ''FeatureFlag
deriveGShow ''FeatureFlag

------------------------------------------------------------------------------
-- Feature flag configuration
------------------------------------------------------------------------------

-- | A snapshot of feature flag configuration
type FeatureFlagMap = DMap FeatureFlag Identity

-- | Feature flag configuration
--
-- This type is tagged with the feature flag, making it difficult to pass the
-- wrong configuration by accident.
--
-- It is necessary to deal with feature flags that do not have configuration.
-- One option is to fail when loading the feature flags.  Another option is to
-- decide what to do at each feature flag usage.  This type supports this
-- option, using 'Nothing' to represent a missing feature flag.
newtype FeatureFlagConfig (tag :: FeatureFlag a)
  = FeatureFlagConfig
    { config :: Maybe a
    }
  deriving (Eq, Show)

-- | Get a boolean feature flag configuration, returning 'True' if missing
configT :: FeatureFlagConfig (tag :: FeatureFlag Bool) -> Bool
configT = fromMaybe True . config

-- | Get a boolean feature flag configuration, returning 'False' if missing
configF :: FeatureFlagConfig (tag :: FeatureFlag Bool) -> Bool
configF = fromMaybe False . config

-- | Lookup a specific feature flag
lookup
  :: FeatureFlag a
  -> FeatureFlagMap
  -> FeatureFlagConfig (tag :: FeatureFlag a)
lookup ff = FeatureFlagConfig . fmap runIdentity . DMap.lookup ff

------------------------------------------------------------------------------
-- Global Storage
------------------------------------------------------------------------------

-- | Global storage for feature flag configuration
globalFeatureFlagMap :: MVar FeatureFlagMap
globalFeatureFlagMap = unsafePerformIO MVar.newEmptyMVar
{-# NOINLINE globalFeatureFlagMap #-}

-- | Initialize the global storage
--
-- The global storage is initialized exactly once in production.  To support
-- testing, however, this function replaces any existing configuration.
initialize :: MonadIO m => FeatureFlagMap -> m ()
initialize ffMap = liftIO $ do
    isSuccess <- MVar.tryPutMVar globalFeatureFlagMap ffMap
    unless isSuccess . void $ MVar.swapMVar globalFeatureFlagMap ffMap

------------------------------------------------------------------------------
-- Usage API
------------------------------------------------------------------------------

-- | Core API for feature flag usage
--
-- This abstraction allows consistent usage with implementations that vary
-- depending on the monad.
--
-- The default implementation is to read the current feature flag
-- configuration from the global storage.  It is implemented as a default so
-- that any other 'MonadIO' monads that should do this can declare an instance
-- trivially and consistently.
class Monad m => MonadFeatureFlags m where
  -- | Get the feature flag configuration
  readM :: m FeatureFlagMap

  default readM :: MonadIO m => m FeatureFlagMap
  readM = liftIO $ MVar.readMVar globalFeatureFlagMap

instance MonadFeatureFlags IO

-- | Get the feature flag configuration and lookup a specific feature flag
lookupM
  :: MonadFeatureFlags m
  => FeatureFlag a
  -> m (FeatureFlagConfig (tag :: FeatureFlag a))
lookupM ff = lookup ff <$> readM

------------------------------------------------------------------------------

-- | Feature flags may be cached within a 'ReaderT' environment
--
-- When using a 'ReaderT' monad with an environment that has the feature
-- flag configuration, 'readM' returns those flags.
class HasFeatureFlags a where
  -- | Get the feature flag configuration
  getFeatureFlags :: a -> FeatureFlagMap

instance HasFeatureFlags FeatureFlagMap where
  getFeatureFlags = id

instance (HasFeatureFlags env, Monad m)
    => MonadFeatureFlags (ReaderT env m) where
  readM = asks getFeatureFlags
